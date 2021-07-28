#include <stdarg.h>
#include "yjit_asm.h"
#include "yjit_utils.h"
#include "yjit_backend.h"

// List of scratch registers
#define NUM_SCR_REGS 2
#define SCR_REGS (ir_opnd_t[]){ IR_REG(RAX), IR_REG(RCX) }

// Branch target that is a code pointer
ir_opnd_t ir_code_ptr(uint8_t* code_ptr)
{
    return (ir_opnd_t) {
        .kind = EIR_CODE_PTR,
        .as.code_ptr = code_ptr
    };
}

// Immediate operand
ir_opnd_t ir_imm(int64_t val)
{
    return (ir_opnd_t) {
        .num_bits = sig_imm_size(val),
        .kind = EIR_IMM,
        .as.imm = val
    };
}

// Create a memory operand with a fixed displacement
ir_opnd_t ir_mem(uint8_t num_bits, ir_opnd_t base, int32_t disp)
{
    RUBY_ASSERT(num_bits == 8 || num_bits == 16 || num_bits == 32 || num_bits == 64);
    RUBY_ASSERT(base.kind == EIR_REG);

    return (ir_opnd_t) {
        .kind = EIR_MEM,
        .num_bits = num_bits,
        .as.mem.base = base.as.reg
    };
}

// Fake/temporaty JIT state object for testing/experimenting
typedef struct yjit_jit_state
{
    insn_array_t insns;

} jitstate_t;

ir_opnd_t push_insn_variadic(jitstate_t* jit, ...)
{
    // Start VA parsing after last named argument
    va_list args;
    va_start(args, jit);

    // Read the opcode
    int op = va_arg(args, int);
    RUBY_ASSERT(op >= 0);
    RUBY_ASSERT(op < OP_MAX);

    ir_insn_t insn = (ir_insn_t){
        .op = op
    };

    // For each operand
    for (size_t opnd_idx = 0;; ++opnd_idx) {
        ir_opnd_t opnd = va_arg(args, ir_opnd_t);

        // End of the operand list
        if (opnd.kind == EIR_VOID)
            break;

        rb_darray_append(&insn.opnds, opnd);
    }

    va_end(args);

    // TODO: we could do some basic validation on the operand count
    // Most opcodes have a fixed operand count

    rb_darray_append(&jit->insns, insn);

    // Return an operand which is the output of this instruction
    return (ir_opnd_t) {
        .kind = EIR_INSN_OUT,
        .as.idx = rb_darray_size(jit->insns) - 1
    };
}

// We use a dummy IR_VOID argument to signal the end of the operands
// Not super safe, but it's the best way I found to deal with the limitations of C99
#define push_insn(jit, ...) push_insn_variadic(jit, __VA_ARGS__, IR_VOID)

bool ir_opnd_eq(ir_opnd_t opnd0, ir_opnd_t opnd1)
{
    if (opnd0.num_bits != opnd1.num_bits)
        return false;

    if (opnd0.kind != opnd1.kind)
        return false;

    // TODO

    return true;
}

/*************************************************/
/* Methods for disassembling the instructions.   */
/*************************************************/

void ir_print_reg(ir_reg_t reg)
{
    if (reg.special)
    {
        if (reg.idx == IR_EC.as.reg.idx)
            printf("EC");
        else if (reg.idx == IR_CFP.as.reg.idx)
            printf("CFP");
        else if (reg.idx == IR_SP.as.reg.idx)
            printf("SP");
        else if (reg.idx == IR_SELF.as.reg.idx)
            printf("SELF");
        else
            RUBY_ASSERT(false);
    }
    else
    {
        printf("R%d", reg.idx);
    }
}

// Print an IR operand
void ir_print_opnd(ir_opnd_t opnd)
{
    switch (opnd.kind)
    {
        case EIR_REG:
            ir_print_reg(opnd.as.reg);
            return;

        case EIR_MEM:
            printf("%db[", (int)opnd.num_bits);
            ir_print_reg(opnd.as.mem.base);
            if (opnd.as.mem.disp > 0)
                printf(" + %d]", opnd.as.mem.disp);
            else
                printf("]");
            return;

        case EIR_IMM:
            printf("%lld", (long long)opnd.as.imm);
            return;

        default:
            RUBY_ASSERT(false && "unknown opnd type");
    }
}

const char* ir_op_name(int op)
{
    switch (op)
    {
        case OP_ADD: return "add";
        case OP_MOV: return "mov";
        case OP_RET: return "ret";

        default:
            rb_bug("unknown opnd type");
    }
}

// Print a list of instructions to stdout for debugging
void ir_print_insns(insn_array_t insns)
{
    // For each instruction
    rb_darray_for(insns, insn_idx)
    {
        ir_insn_t insn = rb_darray_get(insns, insn_idx);
        printf("%s", ir_op_name(insn.op));

        // For each operand
        rb_darray_for(insn.opnds, opnd_idx)
        {
            ir_opnd_t opnd = rb_darray_get(insn.opnds, opnd_idx);

            if (opnd_idx > 0)
                printf(",");
            printf(" ");

            ir_print_opnd(opnd);
        }

        printf(";\n");
    }
}

/*************************************************/
/* Convenience methods for pushing instructions. */
/*************************************************/

ir_opnd_t ir_add(jitstate_t *jit, ir_opnd_t dest, ir_opnd_t src)
{
    return push_insn(jit, OP_ADD, dest, src);
}

ir_opnd_t ir_mov(jitstate_t *jit, ir_opnd_t dest, ir_opnd_t src)
{
    return push_insn(jit, OP_MOV, dest, src);
}

void ir_ret(jitstate_t *jit, ir_opnd_t ret)
{
    push_insn(jit, OP_MOV, IR_REG(RAX), ret);
    push_insn(jit, OP_RET);
}

/*************************************************/
/* Generate x86 code from the IR.                */
/*************************************************/

// A structure for keeping track of the live intervals within the instruction
// list of various variables.
typedef struct yjit_ir_live_interval_t
{
    uint8_t start;
    uint8_t end;
} ir_live_interval_t;

typedef rb_darray(ir_live_interval_t) live_interval_array_t;

x86opnd_t ir_x86_opnd(insn_array_t insns, ir_opnd_t opnd)
{
    switch (opnd.kind)
    {
        case EIR_REG:
            return (x86opnd_t){ OPND_REG, 64, .as.reg = { REG_GP, opnd.as.reg.idx } };
        case EIR_IMM:
            return (x86opnd_t){ OPND_IMM, sig_imm_size(opnd.as.imm), .as.imm = opnd.as.imm };
        case EIR_INSN_OUT: {
            // Temporary cheating way of handling register allocation that will
            // only work for certain instructions.
            ir_insn_t insn = rb_darray_get(insns, opnd.as.idx);
            return ir_x86_opnd(insns, rb_darray_get(insn.opnds, 0));
        }
        default:
            RUBY_ASSERT(false && "unknown opnd kind");
    }
}

void ir_x86_insns(codeblock_t *cb, insn_array_t insns)
{
    cb_set_pos(cb, 0);
    live_interval_array_t live_intervals = (live_interval_array_t){ 0 };

    rb_darray_for(insns, insn_idx)
    {
        ir_insn_t insn = rb_darray_get(insns, insn_idx);

        switch (insn.op)
        {
            case OP_ADD:
                add(cb, ir_x86_opnd(insns, rb_darray_get(insn.opnds, 0)), ir_x86_opnd(insns, rb_darray_get(insn.opnds, 1)));
                break;
            case OP_MOV:
                mov(cb, ir_x86_opnd(insns, rb_darray_get(insn.opnds, 0)), ir_x86_opnd(insns, rb_darray_get(insn.opnds, 1)));
                break;
            case OP_RET:
                ret(cb);
                break;
            default:
                RUBY_ASSERT(false && "unknown op type");
        }
    }
}

/*************************************************/
/* Tests for the backend.                        */
/*************************************************/

void test_backend()
{
    printf("Running backend tests\n");

    jitstate_t jitstate;
    jitstate_t* jit = &jitstate;

    codeblock_t codeblock;
    codeblock_t* cb = &codeblock;

    uint8_t* mem_block = alloc_exec_mem(4096);
    cb_init(cb, mem_block, 4096);

    int (*function)(void);
    function = (int (*)(void))mem_block;

    // Used by the tests to compare function outputs.
    int64_t expected, actual;

    // A macro for defining test cases. Will set up a jitstate, execute the body
    // which should create the IR, generate x86 from the IR, and execute it.
    #define TEST(NAME, EXPECTED, BODY) \
        jitstate = (jitstate_t){ 0 }; \
        BODY \
        ir_x86_insns(cb, jit->insns); \
        expected = EXPECTED; \
        actual = function(); \
        if (expected != actual) { \
            fprintf(stderr, "%s failed: expected %lld, got %lld\n", NAME, expected, actual); \
            exit(-1); \
        }

    TEST("adding with registers", 7, {
        ir_opnd_t opnd0 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_opnd_t opnd1 = ir_mov(jit, IR_REG(RCX), ir_imm(4));
        ir_opnd_t result = ir_add(jit, opnd0, opnd1);
        ir_ret(jit, result);
    })

    TEST("adding with a register and an immediate", 7, {
        ir_opnd_t opnd0 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_opnd_t result = ir_add(jit, opnd0, ir_imm(4));
        ir_ret(jit, result);
    })

    #undef TEST

    printf("Backend tests done\n");

    // This is a rough sketch of what codegen could look like, you can ignore/delete it
    /*
    ir_opnd_t side_exit = ir_code_ptr((void*)0x11ade42bc);

    // Get the operands and destination from the stack
    //ir_opnd_t arg1 = ctx_stack_pop(ctx, 1);
    //ir_opnd_t arg0 = ctx_stack_pop(ctx, 1);

    // 112de41f8:  mov	qword ptr [rdx + 0x10], 5
    push_insn(jit, OP_MOV, opnd0, ir_imm(5));

    //; guard arg0 fixnum
    //112de42e1:  test	byte ptr [rdx - 0x10], 1
    //112de42e5:  je	0x11ade42bc
    push_insn(jit, OP_TEST, opnd0, ir_imm(3));
    push_insn(jit, OP_JUMP_EQ, side_exit);

    //; guard arg1 fixnum
    //112de42eb:  test	byte ptr [rdx - 8], 1
    //112de42ef:  je	0x11ade42bc
    push_insn(jit, OP_TEST, opnd0, ir_imm(3));
    push_insn(jit, OP_JUMP_EQ, side_exit);

    // Value on stack - 1 (untag)
    //112de42f5:  mov	rax, qword ptr [rdx - 0x10]
    //112de42f9:  sub	rax, 1
    push_insn(jit, OP_SUB, opnd0, ir_imm(3));

    //112de42fd:  add	rax, qword ptr [rdx - 8]
    //112de4301:  jo	0x11ade42bc
    ir_opnd_t add_output = push_insn(jit, OP_ADD, opnd1, ir_imm(3));
    push_insn(jit, OP_JUMP_OVF, side_exit);

    // Write output value on stack
    //112de4307:  mov	qword ptr [rdx - 0x10], rax
    push_insn(jit, OP_STORE, opnd_out, add_output);
    */

    //push_insn(jit, OP_CALL, cfunc(rb_foobar), IR_EC, ir_imm(3), ir_imm(3), ir_imm(3));
}
