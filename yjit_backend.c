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

typedef rb_darray(int32_t) live_ranges_t;

// Fake/temporaty JIT state object for testing/experimenting
typedef struct yjit_jit_state
{
    insn_array_t insns;
    live_ranges_t live_ranges;
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

    ir_insn_t insn = (ir_insn_t){ .op = op };
    int32_t insn_index = rb_darray_size(jit->insns);

    // For each operand
    for (size_t opnd_idx = 0;; ++opnd_idx) {
        ir_opnd_t opnd = va_arg(args, ir_opnd_t);

        // If this operand is the output of a previous instruction, then update
        // the end of the live range
        if (opnd.kind == EIR_INSN_OUT)
            rb_darray_set(jit->live_ranges, opnd.as.idx, insn_index);

        // End of the operand list
        if (opnd.kind == EIR_VOID)
            break;

        rb_darray_append(&insn.opnds, opnd);
    }

    va_end(args);

    // TODO: we could do some basic validation on the operand count
    // Most opcodes have a fixed operand count

    rb_darray_append(&jit->insns, insn);

    // Set the initial lifetime of the output of this instruction to just be the
    // current index (as we haven't seen it live beyond that).
    rb_darray_append(&jit->live_ranges, insn_index);

    // Return an operand which is the output of this instruction
    return (ir_opnd_t) { .kind = EIR_INSN_OUT, .as.idx = insn_index };
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

        case EIR_INSN_OUT:
            printf("[%d]", opnd.as.idx);
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
            RUBY_ASSERT(false && "unknown opnd type");
    }
}

// Print a list of instructions to stdout for debugging
void ir_print_insns(jitstate_t *jit)
{
    // For each instruction
    rb_darray_for(jit->insns, insn_idx)
    {
        ir_insn_t insn = rb_darray_get(jit->insns, insn_idx);
        int32_t live_range = rb_darray_get(jit->live_ranges, insn_idx);

        printf("[%d] %s", insn_idx, ir_op_name(insn.op));

        // For each operand
        rb_darray_for(insn.opnds, opnd_idx)
        {
            ir_opnd_t opnd = rb_darray_get(insn.opnds, opnd_idx);

            if (opnd_idx > 0)
                printf(",");
            printf(" ");

            ir_print_opnd(opnd);
        }

        printf(";");

        // If the output of this instruction lives beyond it, then output how
        // long it will stay alive.
        if (live_range != insn_idx)
            printf(" (live until [%d])", live_range);

        printf("\n");
    }
}

void ir_print_to_dot(jitstate_t *jit)
{
    printf("digraph {\n");

    rb_darray_for(jit->insns, insn_idx)
    {
        ir_insn_t insn = rb_darray_get(jit->insns, insn_idx);
        printf("  %d [label=\"[%d] %s\"];\n", insn_idx, insn_idx, ir_op_name(insn.op));

        rb_darray_for(insn.opnds, opnd_idx)
        {
            ir_opnd_t opnd = rb_darray_get(insn.opnds, opnd_idx);
            if (opnd.kind == EIR_INSN_OUT) {
                printf("  %d -> %d\n", opnd.as.idx, insn_idx);
            }
        }
    }

    printf("}\n");
}

/*************************************************/
/* Convenience methods for pushing instructions. */
/*************************************************/

ir_opnd_t ir_add(jitstate_t *jit, ir_opnd_t opnd1, ir_opnd_t opnd2)
{
    return push_insn(jit, OP_ADD, opnd1, opnd2);
}

ir_opnd_t ir_mov(jitstate_t *jit, ir_opnd_t opnd1, ir_opnd_t opnd2)
{
    RUBY_ASSERT((opnd1.kind == EIR_INSN_OUT || opnd1.kind == EIR_REG) && "can only mov into a EIR_INSN_OUT or EIR_REG");
    return push_insn(jit, OP_MOV, opnd1, opnd2);
}

void ir_ret(jitstate_t *jit)
{
    push_insn(jit, OP_RET);
}

ir_opnd_t ir_sub(jitstate_t *jit, ir_opnd_t opnd1, ir_opnd_t opnd2)
{
    return push_insn(jit, OP_SUB, opnd1, opnd2);
}

/*************************************************/
/* Generate x86 code from the IR.                */
/*************************************************/

x86opnd_t ir_x86opnd(x86opnd_t *allocations, ir_opnd_t opnd)
{
    switch (opnd.kind)
    {
        case EIR_REG:
            return (x86opnd_t){ OPND_REG, 64, .as.reg = { REG_GP, opnd.as.reg.idx } };
        case EIR_IMM:
            return (x86opnd_t){ OPND_IMM, opnd.num_bits, .as.imm = opnd.as.imm };
        case EIR_INSN_OUT:
            return allocations[opnd.as.idx];
        default:
            RUBY_ASSERT(false && "unknown opnd kind");
    }
}

#define NOT_LIVE_REG -1

// Fetch a free scratch register and mark it as active.
int32_t ir_next_scratch(int32_t active[NUM_SCR_REGS])
{
    for (int32_t index = 0; index < NUM_SCR_REGS; index++) {
        if (active[index] == NOT_LIVE_REG) {
            return index;
        }
    }
    RUBY_ASSERT(false && "out of free registers");
}

// Allocate a register and mark it as active
x86opnd_t ir_allocate_register(int32_t *active, x86opnd_t *allocations, int32_t insn_idx, int32_t last_insn_index)
{
    int32_t allocated_index = ir_next_scratch(active);
    x86opnd_t allocated = ir_x86opnd(allocations, SCR_REGS[allocated_index]);

    allocations[insn_idx] = allocated;
    active[allocated_index] = last_insn_index;

    return allocated;
}

// Write out x86 instructions into the codeblock for the given IR instructions
void ir_gen_x86(codeblock_t *cb, jitstate_t *jit)
{
    // Initialize a list of integers that corresponds to whether or not the
    // register at that given index is currently active. If it's not the value
    // of NOT_LIVE_REG then it is the instruction index where it will stop being
    // live.
    int32_t active[NUM_SCR_REGS];
    for (int32_t index = 0; index < NUM_SCR_REGS; index++)
        active[index] = NOT_LIVE_REG;

    // This is an array of register allocations for the return variable of each
    // instruction so that they can be referenced later by EIR_INSN_OUT
    // operands.
    int32_t insns_size = rb_darray_size(jit->insns);
    x86opnd_t *allocations = calloc(insns_size, sizeof(x86opnd_t));

    cb_set_pos(cb, 0);

    rb_darray_for(jit->insns, insn_idx)
    {
        // Free the allocated registers back to the not live list if we're past
        // the point where they're last used.
        for (int index = 0; index < NUM_SCR_REGS; index++) {
            if (active[index] != NOT_LIVE_REG && active[index] < insn_idx)
                active[index] = NOT_LIVE_REG;
        }

        ir_insn_t insn = rb_darray_get(jit->insns, insn_idx);
        int32_t last_insn_index = rb_darray_get(jit->live_ranges, insn_idx);

        // You can use the result of this instruction as an operand on a
        // subsequent instruction. When you do, the value of insn_idx (the index
        // of this instruction in the sequence) and the value of last_insn_index
        // (the last index in the sequence where the result of the instruction
        // is used as an operand) will not be the same. In that case we say that
        // this instruction "persists" in that we should ensure we have
        // allocated any necessary registers and made sure the value is staying
        // in the place we expect it to.
        bool persists = insn_idx != last_insn_index;

        switch (insn.op) {
            case OP_ADD: {
                x86opnd_t opnd0 = ir_x86opnd(allocations, rb_darray_get(insn.opnds, 0));
                x86opnd_t opnd1 = ir_x86opnd(allocations, rb_darray_get(insn.opnds, 1));

                if (opnd0.type == OPND_REG) {
                    // If the first operand is already a register, then use that
                    // as the destination for the add instruction.
                    add(cb, opnd0, opnd1);

                    // If this instruction persists through other instructions,
                    // then make sure we move the result into the correct
                    // register.
                    if (persists) {
                        x86opnd_t allocated = ir_allocate_register(active, allocations, insn_idx, last_insn_index);
                        mov(cb, allocated, opnd0);
                    }
                } else if (opnd1.type == OPND_REG) {
                    // If the second operand is already a register, then use
                    // that as the destination for the add instruction.
                    add(cb, opnd1, opnd0);

                    // If this instruction persists through other instructions,
                    // then make sure we move the result into the correct
                    // register.
                    if (persists) {
                        x86opnd_t allocated = ir_allocate_register(active, allocations, insn_idx, last_insn_index);
                        mov(cb, allocated, opnd1);
                    }
                } else {
                    // Since we have two operands that aren't registers, we need
                    // a temporary register to accomplish this instruction, so
                    // here we're going to allocate it regardless of whether or
                    // not this instruction result persists.
                    x86opnd_t allocated = ir_allocate_register(active, allocations, insn_idx, last_insn_index);
                    mov(cb, allocated, opnd0);
                    add(cb, allocated, opnd1);
                }
                break;
            }
            case OP_MOV: {
                // We're assuming here that opnd0 is a register that we can mov
                // opnd1 into.
                x86opnd_t opnd0 = ir_x86opnd(allocations, rb_darray_get(insn.opnds, 0));
                x86opnd_t opnd1 = ir_x86opnd(allocations, rb_darray_get(insn.opnds, 1));

                // If this instruction result persists beyond this instruction,
                // it's going to need to know the allocation. We don't actually
                // need to allocate anything since we know we're already moving
                // a value into a register, so here we're just going to mark the
                // allocation as whatever is living in the first operand.
                if (persists)
                    allocations[insn_idx] = opnd0;

                mov(cb, opnd0, opnd1);
                break;
            }
            case OP_RET:
                ret(cb);
                break;
            default:
                RUBY_ASSERT(false && "unsupported insn op");
        }
    }

    free(allocations);
}

/*************************************************/
/* Tests for the backend.                        */
/*************************************************/

void test_backend()
{
    jitstate_t jitstate;
    jitstate_t* jit = &jitstate;

    codeblock_t codeblock;
    codeblock_t* cb = &codeblock;

    uint8_t* mem_block = alloc_exec_mem(4096);
    cb_init(cb, mem_block, 4096);

    int (*function)(void);
    function = (int (*)(void))mem_block;

    printf("Running backend tests\n");

    // Used by the tests to compare function outputs.
    int64_t expected, actual;

    // A macro for defining test cases. Will set up a jitstate, execute the body
    // which should create the IR, generate x86 from the IR, and execute it.
    #define TEST(NAME, EXPECTED, BODY) \
        jitstate = (jitstate_t){ 0 }; \
        BODY \
        ir_gen_x86(cb, jit); \
        expected = EXPECTED; \
        actual = function(); \
        if (expected != actual) { \
            fprintf(stderr, "%s failed: expected %lld, got %lld\n", NAME, expected, actual); \
            exit(-1); \
        }

    TEST("adding with registers", 7, {
        ir_opnd_t opnd0 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_opnd_t opnd1 = ir_mov(jit, IR_REG(RCX), ir_imm(4));
        ir_mov(jit, IR_REG(RAX), ir_add(jit, opnd0, opnd1));
        ir_ret(jit);
    })

    TEST("adding with a register and an immediate", 7, {
        ir_opnd_t opnd0 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_mov(jit, IR_REG(RAX), ir_add(jit, opnd0, ir_imm(4)));
        ir_ret(jit);
    })

    TEST("adding with an immediate and a register", 7, {
        ir_opnd_t opnd1 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_mov(jit, IR_REG(RAX), ir_add(jit, ir_imm(4), opnd1));
        ir_ret(jit);
    })

    TEST("adding with both immediates", 7, {
        ir_mov(jit, IR_REG(RAX), ir_add(jit, ir_imm(3), ir_imm(4)));
        ir_ret(jit);
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
