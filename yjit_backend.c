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

ir_opnd_t ir_const_ptr(void *ptr)
{
    return (ir_opnd_t) {
        .num_bits = sig_imm_size((uint64_t) ptr),
        .kind = EIR_IMM,
        .as.u_imm = (uint64_t) ptr
    };
}

ir_opnd_t ir_str_ptr(char *str)
{
    return (ir_opnd_t) {
        .num_bits = sig_imm_size((uint64_t) str),
        .kind = EIR_IMM,
        .as.str = str
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

ir_opnd_t ir_label_opnd(char *name)
{
    return (ir_opnd_t) {
        .kind = EIR_LABEL_NAME,
        .as.str = name
    };
}

typedef rb_darray(int32_t) live_ranges_t;

// Fake/temporary JIT state object for testing/experimenting
// This object only exists during code generation
typedef struct yjit_jit_state
{
    // The current list of instructions that will be used to write out the
    // generated assembly code
    insn_array_t insns;

    // The previous list of instructions, used for optimization/allocation
    // passes
    insn_array_t insns_prev;

    // Metadata about how long return value operands (EIR_INSN_OUT) are "live"
    // so that we can perform register allocation properly
    live_ranges_t live_ranges;
} jitstate_t;

ir_opnd_t ir_push_insn_variadic(jitstate_t* jit, ...)
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
#define ir_push_insn(jit, ...) ir_push_insn_variadic(jit, __VA_ARGS__, IR_VOID)

bool ir_reg_eq(ir_reg_t reg0, ir_reg_t reg1)
{
    return reg0.idx == reg1.idx && reg0.special == reg1.special;
}

bool ir_opnd_eq(ir_opnd_t opnd0, ir_opnd_t opnd1)
{
    if (opnd0.num_bits != opnd1.num_bits)
        return false;

    if (opnd0.kind != opnd1.kind)
        return false;

    switch (opnd0.kind)
    {
        case EIR_MEM:
            return (
                opnd0.as.mem.disp == opnd1.as.mem.disp &&
                ir_reg_eq(opnd0.as.mem.base, opnd1.as.mem.base)
            );
        case EIR_REG:
            return ir_reg_eq(opnd0.as.reg, opnd1.as.reg);
        default:
            RUBY_ASSERT(false && "unsupported opnd type");
    }

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
            printf("\033[32mEC\033[0m");
        else if (reg.idx == IR_CFP.as.reg.idx)
            printf("\033[32mCFP\033[0m");
        else if (reg.idx == IR_SP.as.reg.idx)
            printf("\033[32mSP\033[0m");
        else if (reg.idx == IR_SELF.as.reg.idx)
            printf("\033[32mSELF\033[0m");
        else
            RUBY_ASSERT(false);
    }
    else
    {
        printf("\033[32mR%d\033[0m", reg.idx);
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
            printf("\033[33m%db[", (int)opnd.num_bits);
            ir_print_reg(opnd.as.mem.base);
            if (opnd.as.mem.disp > 0)
                printf(" + %d]\033[0m", opnd.as.mem.disp);
            else
                printf("]\033[0m");
            return;

        case EIR_IMM:
            printf("\033[34m%lld\033[0m", (long long)opnd.as.imm);
            return;

        case EIR_INSN_OUT:
            printf("i%03d", opnd.as.idx);
            return;

        case EIR_LABEL_NAME:
            printf("%s", opnd.as.str);
            return;

        case EIR_LABEL_IDX:
            printf("%d", opnd.as.idx);
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
        case OP_AND: return "and";
        case OP_CALL: return "call";
        case OP_CCALL: return "ccall";
        case OP_CMOV_GE: return "cmovge";
        case OP_CMOV_GT: return "cmovg";
        case OP_CMOV_LE: return "cmovle";
        case OP_CMOV_LT: return "cmovl";
        case OP_CMP: return "cmp";
        case OP_COMMENT: return "comment";
        case OP_JUMP_EQ: return "jumpeq";
        case OP_JUMP_NE: return "jumpne";
        case OP_JUMP_OVF: return "jumpovf";
        case OP_LABEL: return "label";
        case OP_MOV: return "mov";
        case OP_NOT: return "not";
        case OP_RET: return "ret";
        case OP_RETVAL: return "retval";
        case OP_SELECT_GE: return "selectge";
        case OP_SELECT_GT: return "selectgt";
        case OP_SELECT_LE: return "selectle";
        case OP_SELECT_LT: return "selectlt";
        case OP_SUB: return "sub";

        default:
            RUBY_ASSERT(false && "unknown opnd type");
            return "unknown";
    }
}

// Print a list of instructions to stdout for debugging
void ir_print_insns(jitstate_t *jit)
{
    rb_darray_for(jit->insns, insn_idx)
    {
        ir_insn_t insn = rb_darray_get(jit->insns, insn_idx);
        printf("i%03d ", insn_idx);

        if (insn.op == OP_COMMENT) {
            printf("\033[3m; %s\033[0m\n", rb_darray_get(insn.opnds, 0).as.str);
        } else {
            printf("\033[31m%s\033[0m", ir_op_name(insn.op));

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

#define ir_add(jit, opnd0, opnd1) ir_push_insn(jit, OP_ADD, opnd0, opnd1)

#define ir_and(jit, opnd0, opnd1) ir_push_insn(jit, OP_AND, opnd0, opnd1)

#define ir_ccall(jit, ...) ir_push_insn(jit, OP_CCALL, __VA_ARGS__)

#define ir_cmov_ge(jit, opnd0, opnd1) \
    ir_push_insn(jit, OP_CMOV_GE, opnd0, opnd1)

#define ir_cmov_gt(jit, opnd0, opnd1) \
    ir_push_insn(jit, OP_CMOV_GT, opnd0, opnd1)

#define ir_cmov_le(jit, opnd0, opnd1) \
    ir_push_insn(jit, OP_CMOV_LE, opnd0, opnd1)

#define ir_cmov_lt(jit, opnd0, opnd1) \
    ir_push_insn(jit, OP_CMOV_LT, opnd0, opnd1)

void ir_comment(jitstate_t *jit, ir_opnd_t opnd)
{
    ir_push_insn(jit, OP_COMMENT, opnd);
}

void ir_jump_eq(jitstate_t *jit, ir_opnd_t opnd0, ir_opnd_t opnd1, ir_opnd_t label_opnd)
{
    RUBY_ASSERT((label_opnd.kind == EIR_LABEL_NAME) && "can only jump with an EIR_LABEL_NAME");
    ir_push_insn(jit, OP_JUMP_EQ, opnd0, opnd1, label_opnd);
}

void ir_jump_ne(jitstate_t *jit, ir_opnd_t opnd0, ir_opnd_t opnd1, ir_opnd_t label_opnd)
{
    RUBY_ASSERT((label_opnd.kind == EIR_LABEL_NAME) && "can only jump with an EIR_LABEL_NAME");
    ir_push_insn(jit, OP_JUMP_NE, opnd0, opnd1, label_opnd);
}

void ir_jump_ovf(jitstate_t *jit, ir_opnd_t label_opnd)
{
    RUBY_ASSERT((label_opnd.kind == EIR_LABEL_NAME) && "can only jump with an EIR_LABEL_NAME");
    ir_push_insn(jit, OP_JUMP_OVF, label_opnd);
}

void ir_label(jitstate_t *jit, ir_opnd_t label_opnd)
{
    RUBY_ASSERT((label_opnd.kind == EIR_LABEL_NAME) && "can only label with an EIR_LABEL_NAME");
    ir_push_insn(jit, OP_LABEL, label_opnd);
}

ir_opnd_t ir_mov(jitstate_t *jit, ir_opnd_t opnd0, ir_opnd_t opnd1)
{
    RUBY_ASSERT((opnd0.kind == EIR_INSN_OUT || opnd0.kind == EIR_REG) && "can only mov into a EIR_INSN_OUT or EIR_REG");
    return ir_push_insn(jit, OP_MOV, opnd0, opnd1);
}

ir_opnd_t ir_cmp(jitstate_t *jit, ir_opnd_t opnd0, ir_opnd_t opnd1)
{
    RUBY_ASSERT((opnd0.kind == EIR_INSN_OUT || opnd0.kind == EIR_REG) && "can only cmp with a EIR_INSN_OUT or EIR_REG");
    return ir_push_insn(jit, OP_CMP, opnd0, opnd1);
}

#define ir_not(jit, opnd) ir_push_insn(jit, OP_NOT, opnd)

void ir_ret(jitstate_t *jit)
{
    ir_push_insn(jit, OP_RET);
}

void ir_retval(jitstate_t *jit, ir_opnd_t opnd)
{
    ir_push_insn(jit, OP_RETVAL, opnd);
}

#define ir_select_ge(jit, opnd0, opnd1, opnd2, opnd3) \
    ir_push_insn(jit, OP_SELECT_GE, opnd0, opnd1, opnd2, opnd3)

#define ir_select_gt(jit, opnd0, opnd1, opnd2, opnd3) \
    ir_push_insn(jit, OP_SELECT_GT, opnd0, opnd1, opnd2, opnd3)

#define ir_select_le(jit, opnd0, opnd1, opnd2, opnd3) \
    ir_push_insn(jit, OP_SELECT_LE, opnd0, opnd1, opnd2, opnd3)

#define ir_select_lt(jit, opnd0, opnd1, opnd2, opnd3) \
    ir_push_insn(jit, OP_SELECT_LT, opnd0, opnd1, opnd2, opnd3)

#define ir_sub(jit, opnd0, opnd1) ir_push_insn(jit, OP_SUB, opnd0, opnd1)

/*************************************************/
/* Register allocation.                          */
/*************************************************/

#define NOT_LIVE_REG -1

ir_opnd_t ir_opnd(ir_insn_t insn, int32_t opnd_idx, int32_t *allocations)
{
    ir_opnd_t opnd = rb_darray_get(insn.opnds, opnd_idx);
    return opnd.kind == EIR_INSN_OUT ? SCR_REGS[allocations[opnd.as.idx]] : opnd;
}

// Find the index of the next scratch register that can be allocated
int32_t ir_next_scr_reg_idx(int32_t active[NUM_SCR_REGS])
{
    for (int32_t index = 0; index < NUM_SCR_REGS; index++) {
        if (active[index] == NOT_LIVE_REG) {
            return index;
        }
    }

    RUBY_ASSERT(false && "out of free registers");
    return -1;
}

// When we're about to walk through the instructions and perform some kind of
// optimization/allocation/lowering pass, we need to swap the current
// instructions with the buffer so that when we push on new instructions they go
// to the correct place.
void ir_swap_insns(jitstate_t *jit)
{
    // Grab a reference to the previous set instructions before we swap them.
    insn_array_t insns = jit->insns_prev;

    // Set the previous instructions to the current ones and set the current
    // ones to a new blank darray.
    jit->insns_prev = jit->insns;
    jit->insns = (insn_array_t) { 0 };

    // If there were previous instructions, then clear them out. (This will
    // only occur on the second pass and onward.)
    if (insns)
        rb_darray_clear(insns);
}

void ir_insn_free(ir_insn_t insn) {
    rb_darray_free(insn.opnds);
}

void ir_jit_clear(jitstate_t *jit) {
    ir_insn_t *insn;
    rb_darray_foreach(jit->insns, insn_idx, insn) ir_insn_free(*insn);

    rb_darray_clear(jit->insns);
    rb_darray_clear(jit->insns_prev);
}

void ir_alloc_regs(jitstate_t *jit)
{
    ir_swap_insns(jit);
    ir_insn_t *insn_ptr;

    // Initialize a list of integers that corresponds to whether or not the
    // register at that given index is currently active. If it's not the value
    // of NOT_LIVE_REG then it is the instruction index where it will stop being
    // live.
    int32_t active[NUM_SCR_REGS];
    for (int32_t index = 0; index < NUM_SCR_REGS; index++)
        active[index] = NOT_LIVE_REG;

    // This is an array of integers that correspond to indices in the SCR_REG
    // list. They track the operand that results from each instruction.
    int32_t *allocations = calloc(rb_darray_size(jit->insns_prev), sizeof(int32_t));

    rb_darray_foreach(jit->insns_prev, insn_idx, insn_ptr)
    {
        // Free the allocated registers back to the not live list if we're past
        // the point where they're last used.
        for (int index = 0; index < NUM_SCR_REGS; index++) {
            if (active[index] != NOT_LIVE_REG && active[index] < insn_idx)
                active[index] = NOT_LIVE_REG;
        }

        ir_insn_t insn = *insn_ptr;
        int32_t last_insn_index = rb_darray_get(jit->live_ranges, insn_idx);

        switch (insn.op) {
            case OP_ADD:
            case OP_AND:
            case OP_SUB: {
                ir_opnd_t opnd0 = ir_opnd(insn, 0, allocations);
                ir_opnd_t opnd1 = ir_opnd(insn, 1, allocations);

                if (opnd0.kind != EIR_REG && opnd1.kind != EIR_REG) {
                    // Since we have two operands that aren't registers, we need
                    // a temporary register to accomplish this instruction, so
                    // here we're going to allocate it.
                    int32_t allocated_index = ir_next_scr_reg_idx(active);
                    ir_opnd_t allocated = SCR_REGS[allocated_index];

                    allocations[insn_idx] = allocated_index;
                    active[allocated_index] = last_insn_index;

                    ir_mov(jit, allocated, opnd0);
                    ir_push_insn(jit, insn.op, allocated, opnd1);
                } else {
                    // Since at least one of the two operands is a register,
                    // we're going to use that operand as the destination of
                    // this instruction and skip needing to allocate one.
                    ir_opnd_t dest = opnd0;
                    ir_opnd_t src = opnd1;

                    if (opnd0.kind != EIR_REG) {
                        dest = opnd1;
                        src = opnd0;
                    }

                    // If we're about to move the value into one of our scratch
                    // registers, then we should mark it as active until the
                    // result of this instruction is not longer needed.
                    for (int32_t index = 0; index < NUM_SCR_REGS; index++) {
                        if (ir_opnd_eq(SCR_REGS[index], dest)) {
                            allocations[insn_idx] = index;
                            active[index] = last_insn_index;
                            break;
                        }
                    }

                    ir_push_insn(jit, insn.op, dest, src);
                }
                break;
            }
            case OP_MOV: {
                // We're assuming here that opnd0 is a register that we can mov
                // opnd1 into.
                ir_opnd_t opnd0 = ir_opnd(insn, 0, allocations);
                ir_opnd_t opnd1 = ir_opnd(insn, 1, allocations);

                // If the values are already equal, then there's no need to add
                // a mov instruction to the list.
                if (ir_opnd_eq(opnd0, opnd1))
                    break;

                // If we're about to move the value into one of our scratch
                // registers, then we should mark it as active until the result
                // of this instruction is not longer needed.
                for (int32_t index = 0; index < NUM_SCR_REGS; index++) {
                    if (ir_opnd_eq(SCR_REGS[index], opnd0)) {
                        allocations[insn_idx] = index;
                        active[index] = last_insn_index;
                        break;
                    }
                }

                ir_mov(jit, opnd0, opnd1);
                break;
            }
            case OP_NOT:
            case OP_RETVAL: {
                ir_opnd_t opnd = ir_opnd(insn, 0, allocations);

                if (opnd.kind != EIR_REG) {
                    // Since we have an operand that isn't a register, we need a
                    // temporary register to accomplish this instruction, so
                    // here we're going to allocate it.
                    int32_t allocated_index = ir_next_scr_reg_idx(active);
                    ir_opnd_t allocated = SCR_REGS[allocated_index];

                    allocations[insn_idx] = allocated_index;
                    active[allocated_index] = last_insn_index;

                    ir_mov(jit, allocated, opnd);
                    ir_push_insn(jit, insn.op, allocated);
                } else {
                    // Since the operand is a register, we can just directly not
                    // that register. If we're about to use a register that is
                    // one of our scratch registers, then we should mark it as
                    // active until the result of this instruction is not longer
                    // needed.
                    for (int32_t index = 0; index < NUM_SCR_REGS; index++) {
                        if (ir_opnd_eq(SCR_REGS[index], opnd)) {
                            allocations[insn_idx] = index;
                            active[index] = last_insn_index;
                            break;
                        }
                    }

                    ir_push_insn(jit, insn.op, opnd);
                }
                break;
            }
            case OP_JUMP_EQ:
            case OP_JUMP_NE: {
                ir_opnd_t opnd0 = ir_opnd(insn, 0, allocations);
                ir_opnd_t opnd1 = ir_opnd(insn, 1, allocations);
                ir_opnd_t label_opnd = ir_opnd(insn, 2, allocations);

                if (opnd0.kind != EIR_REG && opnd1.kind != EIR_REG) {
                    // Since we have two operands that aren't registers, we need
                    // a temporary register to accomplish this instruction, so
                    // here we're going to allocate it.
                    int32_t allocated_index = ir_next_scr_reg_idx(active);
                    ir_opnd_t allocated = SCR_REGS[allocated_index];

                    allocations[insn_idx] = allocated_index;
                    active[allocated_index] = last_insn_index;

                    ir_mov(jit, allocated, opnd0);
                    ir_push_insn(jit, insn.op, allocated, opnd1, label_opnd);
                } else {
                    // Since at least one of the two operands is a register,
                    // we're going to use that operand as the destination of
                    // this instruction and skip needing to allocate one.
                    ir_opnd_t dest = opnd0;
                    ir_opnd_t src = opnd1;

                    if (opnd0.kind != EIR_REG) {
                        dest = opnd1;
                        src = opnd0;
                    }

                    // If we're about to move the value into one of our scratch
                    // registers, then we should mark it as active until the
                    // result of this instruction is not longer needed.
                    for (int32_t index = 0; index < NUM_SCR_REGS; index++) {
                        if (ir_opnd_eq(SCR_REGS[index], dest)) {
                            allocations[insn_idx] = index;
                            active[index] = last_insn_index;
                            break;
                        }
                    }

                    ir_push_insn(jit, insn.op, dest, src, label_opnd);
                }

                break;
            }
            case OP_CCALL: {
                ir_opnd_t function = ir_opnd(insn, 0, allocations);
                ir_opnd_t pointer = function;

                if (function.kind != EIR_REG) {
                    // Since we don't have as register as the operand that will
                    // hold the pointer to the function, we have to first
                    // allocate one and then mov the function pointer into that
                    // register.
                    int32_t allocated_index = ir_next_scr_reg_idx(active);
                    ir_opnd_t allocated = SCR_REGS[allocated_index];

                    allocations[insn_idx] = allocated_index;
                    active[allocated_index] = last_insn_index;

                    ir_mov(jit, allocated, function);
                    pointer = allocated;
                }

                // Create the next instruction manually (doing this instead of
                // using ir_push_insn since this function accepts variadic
                // arguments).
                ir_insn_t call = (ir_insn_t){ .op = OP_CALL };
                rb_darray_append(&call.opnds, pointer);

                // Starting at index 1 because we're skipping over the function
                // pointer operand.
                int32_t opnd_size = rb_darray_size(insn.opnds);
                for (int32_t opnd_idx = 1; opnd_idx < opnd_size; opnd_idx++) {
                    rb_darray_append(&call.opnds, ir_opnd(insn, opnd_idx, allocations));
                }

                rb_darray_append(&jit->insns, call);
                break;
            }
            case OP_SELECT_GE:
            case OP_SELECT_GT:
            case OP_SELECT_LE:
            case OP_SELECT_LT: {
                ir_opnd_t left = ir_opnd(insn, 0, allocations);
                ir_opnd_t right = ir_opnd(insn, 1, allocations);

                ir_opnd_t then_case = ir_opnd(insn, 2, allocations);
                ir_opnd_t else_case = ir_opnd(insn, 3, allocations);

                // Always allocate a register, since we need a place for the
                // cases to live.
                int32_t allocated_index = ir_next_scr_reg_idx(active);
                ir_opnd_t allocated = SCR_REGS[allocated_index];

                allocations[insn_idx] = allocated_index;
                active[allocated_index] = last_insn_index;

                ir_mov(jit, allocated, left);
                ir_cmp(jit, allocated, right);

                // Now that the comparison is done, we can safely move the
                // else_case operand into the allocated register while
                // maintaining the comparison flags.
                ir_mov(jit, allocated, else_case);

                if (then_case.kind != EIR_REG) {
                    // Since we don't have as register as the then_case operand,
                    // we have to first allocate one and then mov the value into
                    // that register.
                    int32_t then_allocated_index = ir_next_scr_reg_idx(active);
                    ir_opnd_t then_allocated = SCR_REGS[then_allocated_index];

                    active[then_allocated_index] = last_insn_index;
                    ir_mov(jit, then_allocated, then_case);
                    then_case = then_allocated;
                }

                // Now we're going to conditionally move the then_case into the
                // allocated register depending on if the comparison flags line
                // up to the expected values depending on the current
                // instruction.
                switch (insn.op) {
                    case OP_SELECT_GE: ir_cmov_ge(jit, allocated, then_case); break;
                    case OP_SELECT_GT: ir_cmov_gt(jit, allocated, then_case); break;
                    case OP_SELECT_LE: ir_cmov_le(jit, allocated, then_case); break;
                    case OP_SELECT_LT: ir_cmov_lt(jit, allocated, then_case); break;
                }

                break;
            }
        }

        switch (insn.op) {
            // These instructions are just copied over between passes.
            case OP_CALL:
            case OP_CMOV_GE:
            case OP_CMOV_GT:
            case OP_CMOV_LE:
            case OP_CMOV_LT:
            case OP_CMP:
            case OP_COMMENT:
            case OP_JUMP_OVF:
            case OP_LABEL:
            case OP_RET: {
                rb_darray_append(&jit->insns, insn);
                break;
            }
            // By default we're going to free the previous instruction.
            default:
                ir_insn_free(insn);
                break;
        }
    }

    free(allocations);
}

// This pass doesn't actually do anything at the moment, it's just here to show
// how we would do additional passes through the IR.
void ir_peephole_opt(jitstate_t *jit)
{
    ir_swap_insns(jit);
    ir_insn_t *insn;

    rb_darray_foreach(jit->insns_prev, insn_idx, insn)
        rb_darray_append(&jit->insns, *insn);
}

/*************************************************/
/* Generate x86 code from the IR.                */
/*************************************************/

x86opnd_t ir_gen_x86opnd(ir_opnd_t opnd)
{
    switch (opnd.kind)
    {
        case EIR_REG:
            return (x86opnd_t){ OPND_REG, opnd.num_bits, .as.reg = { REG_GP, opnd.as.reg.idx } };
        case EIR_IMM:
            return (x86opnd_t){ OPND_IMM, opnd.num_bits, .as.imm = opnd.as.imm };
        default:
            RUBY_ASSERT(false && "unknown opnd kind");
            return (x86opnd_t){ 0 };
    }
}

// Write out x86 instructions into the codeblock for the given IR instructions
void ir_gen_x86(codeblock_t *cb, jitstate_t *jit)
{
    ir_insn_t *insn;
    ir_opnd_t *opnd;

    // First we need to loop through and register every label with the X86
    // codeblock. Since the operand structs have different addresses, we're
    // keying off the address of the constant string name.
    rb_darray_foreach(jit->insns, insn_idx, insn)
    {
        rb_darray_foreach(insn->opnds, opnd_idx, opnd)
        {
            if (opnd->kind != EIR_LABEL_NAME) {
                continue;
            }

            // Find out if we're already set an index for this label. If we
            // have, then set the index to that value.
            uint32_t label_idx;
            for (label_idx = 0; label_idx < cb->num_labels; label_idx++) {
                if (cb->label_names[label_idx] == opnd->as.str) {
                    opnd->kind = EIR_LABEL_IDX;
                    opnd->as.idx = label_idx;
                }
            }

            // Otherwise, allocate the new label and mark it down in our list.
            if (opnd->kind == EIR_LABEL_NAME) {
                opnd->kind = EIR_LABEL_IDX;
                opnd->as.idx = cb_new_label(cb, opnd->as.str);
            }
        }
    }

    // Now loop through each instruction and generate corresponding X86
    // instructions. This should be pretty much a one-to-one mapping, if
    // something more complicated needs to occur it should happen in a
    // preceeding pass.
    rb_darray_foreach(jit->insns, insn_idx, insn)
    {
        switch (insn->op) {
            case OP_ADD: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                add(cb, opnd0, opnd1);
                break;
            }
            case OP_AND: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                and(cb, opnd0, opnd1);
                break;
            }
            case OP_CALL: {
                x86opnd_t pointer = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                int32_t opnd_size = rb_darray_size(insn->opnds);

                if (opnd_size > NUM_C_ARG_REGS) {
                    RUBY_ASSERT(false && "unsupported function argument spill");
                }

                // TODO: some of these movs may not be necessary, and some of
                // them may overwrite values we need to keep. Presumably there
                // should be some smart pushing/popping going on here. At the
                // moment we're just blinding copying values into the correct
                // registers.
                for (int32_t opnd_idx = 1; opnd_idx < opnd_size; opnd_idx++) {
                    x86opnd_t opnd = ir_gen_x86opnd(rb_darray_get(insn->opnds, opnd_idx));
                    mov(cb, C_ARG_REGS[opnd_idx - 1], opnd);
                }

                call(cb, pointer);
                break;
            }
            case OP_CMOV_GE: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                cmovge(cb, opnd0, opnd1);
                break;
            }
            case OP_CMOV_GT: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                cmovg(cb, opnd0, opnd1);
                break;
            }
            case OP_CMOV_LE: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                cmovle(cb, opnd0, opnd1);
                break;
            }
            case OP_CMOV_LT: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                cmovl(cb, opnd0, opnd1);
                break;
            }
            case OP_CMP: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                cmp(cb, opnd0, opnd1);
                break;
            }
            case OP_COMMENT:
                // Do nothing here until/unless we have a way to consistently
                // represent comments in the assembly
                break;
            case OP_JUMP_EQ: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                ir_opnd_t label_opnd = rb_darray_get(insn->opnds, 2);
                cmp(cb, opnd0, opnd1);
                je_label(cb, label_opnd.as.idx);
                break;
            }
            case OP_JUMP_NE: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                ir_opnd_t label_opnd = rb_darray_get(insn->opnds, 2);
                cmp(cb, opnd0, opnd1);
                jne_label(cb, label_opnd.as.idx);
                break;
            }
            case OP_JUMP_OVF: {
                ir_opnd_t label_opnd = rb_darray_get(insn->opnds, 0);
                jo_label(cb, label_opnd.as.idx);
                break;
            }
            case OP_LABEL: {
                ir_opnd_t label_opnd = rb_darray_get(insn->opnds, 0);
                cb_write_label(cb, label_opnd.as.idx);
                break;
            }
            case OP_MOV: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                mov(cb, opnd0, opnd1);
                break;
            }
            case OP_NOT: {
                x86opnd_t opnd = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                not(cb, opnd);
                break;
            }
            case OP_RET:
                ret(cb);
                break;
            case OP_RETVAL: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                mov(cb, RAX, opnd0); // TODO: check if this is necessary
                ret(cb);
                break;
            }
            case OP_SUB: {
                x86opnd_t opnd0 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 0));
                x86opnd_t opnd1 = ir_gen_x86opnd(rb_darray_get(insn->opnds, 1));
                sub(cb, opnd0, opnd1);
                break;
            }
            default:
                RUBY_ASSERT(false && "unsupported insn op");
        }
    }

    cb_link_labels(cb);
}

/*************************************************/
/* Tests for the backend.                        */
/*************************************************/

// These are just here to have functions to call when testing call instructions
static int64_t ir_test_function_plain()
{
    return 7;
}

static int64_t ir_test_function_add(int64_t left, int64_t right)
{
    return left + right;
}

void test_backend()
{
    codeblock_t codeblock;
    codeblock_t* cb = &codeblock;

    uint8_t* mem_block = alloc_exec_mem(4096);
    cb_init(cb, mem_block, 4096);

    int (*function)(void);
    function = (int (*)(void))mem_block;

    printf("Running backend tests\n");

    // Used by the tests to compare function outputs.
    int expected, actual;

    // Used by the tests to write out the IR instructions.
    jitstate_t jitstate;
    jitstate_t* jit = &jitstate;

    // A macro for defining test cases. Will set up a jitstate, execute the body
    // which should create the IR, generate x86 from the IR, and execute it.
    #define TEST(NAME, EXPECTED, BODY) \
        cb_set_pos(cb, 0); \
        jitstate = (jitstate_t){ 0, 0, 0 }; \
        BODY \
        ir_alloc_regs(jit); \
        ir_peephole_opt(jit); \
        ir_gen_x86(cb, jit); \
        ir_jit_clear(jit); \
        expected = EXPECTED; \
        actual = function(); \
        if (expected != actual) { \
            fprintf(stderr, "%s failed: expected %d, got %d\n", NAME, expected, actual); \
            exit(-1); \
        }

    TEST("basic returning", 3, {
        ir_retval(jit, ir_imm(3));
    })

    TEST("adding with registers", 7, {
        ir_opnd_t opnd0 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_opnd_t opnd1 = ir_mov(jit, IR_REG(RCX), ir_imm(4));
        ir_retval(jit, ir_add(jit, opnd0, opnd1));
    })

    TEST("adding with a register and an immediate", 7, {
        ir_opnd_t opnd0 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_retval(jit, ir_add(jit, opnd0, ir_imm(4)));
    })

    TEST("adding with an immediate and a register", 7, {
        ir_opnd_t opnd1 = ir_mov(jit, IR_REG(RAX), ir_imm(3));
        ir_retval(jit, ir_add(jit, ir_imm(4), opnd1));
    })

    TEST("adding with both immediates", 7, {
        ir_retval(jit, ir_add(jit, ir_imm(3), ir_imm(4)));
    })

    TEST("add", 10, {
        ir_retval(jit, ir_add(jit, ir_add(jit, ir_imm(3), ir_imm(4)), ir_imm(3)));
    })

    TEST("sub", 10, {
        ir_opnd_t opnd0 = ir_add(jit, ir_imm(3), ir_imm(4));
        ir_opnd_t opnd1 = ir_sub(jit, ir_imm(5), ir_imm(2));
        ir_retval(jit, ir_add(jit, opnd0, opnd1));
    })

    TEST("and", 4, {
        ir_opnd_t opnd0 = ir_add(jit, ir_imm(2), ir_imm(3));
        ir_opnd_t opnd1 = ir_sub(jit, ir_imm(18), ir_imm(4));
        ir_retval(jit, ir_and(jit, opnd0, opnd1));
    });

    TEST("not", -11, {
        ir_retval(jit, ir_not(jit, ir_imm(10)));
    });

    TEST("jump equal", 2, {
        ir_opnd_t label = ir_label_opnd((char *) "label");
        ir_jump_eq(jit, ir_imm(3), ir_imm(3), label);
        ir_retval(jit, ir_imm(1));
        ir_label(jit, label);
        ir_retval(jit, ir_imm(2));
    });

    TEST("jump not equal", 2, {
        ir_opnd_t label = ir_label_opnd((char *) "label");
        ir_jump_ne(jit, ir_imm(3), ir_imm(4), label);
        ir_retval(jit, ir_imm(1));
        ir_label(jit, label);
        ir_retval(jit, ir_imm(2));
    });

    TEST("jump overflow", 2, {
        ir_opnd_t label = ir_label_opnd((char *) "label");
        ir_add(jit, ir_imm(INT64_MAX), ir_imm(1));
        ir_jump_ovf(jit, label);
        ir_retval(jit, ir_imm(1));
        ir_label(jit, label);
        ir_retval(jit, ir_imm(2));
    })

    TEST("call plain", 7, {
        ir_retval(jit, ir_ccall(jit, ir_const_ptr((void *)&ir_test_function_plain)));
    });

    TEST("call with arguments", 7, {
        ir_retval(jit, ir_ccall(jit, ir_const_ptr((void *)&ir_test_function_add), ir_imm(3), ir_imm(4)));
    });

    TEST("select greater than or equal to", 1, {
        ir_retval(jit, ir_select_ge(jit, ir_imm(3), ir_imm(3), ir_imm(1), ir_imm(2)));
    })

    TEST("select not greater than or equal to", 2, {
        ir_retval(jit, ir_select_ge(jit, ir_imm(3), ir_imm(4), ir_imm(1), ir_imm(2)));
    })

    TEST("select greater than", 1, {
        ir_retval(jit, ir_select_gt(jit, ir_imm(4), ir_imm(3), ir_imm(1), ir_imm(2)));
    })

    TEST("select not greater than", 2, {
        ir_retval(jit, ir_select_gt(jit, ir_imm(3), ir_imm(3), ir_imm(1), ir_imm(2)));
    })

    TEST("select less than or equal to", 1, {
        ir_retval(jit, ir_select_le(jit, ir_imm(3), ir_imm(3), ir_imm(1), ir_imm(2)));
    })

    TEST("select not less than or equal to", 2, {
        ir_retval(jit, ir_select_le(jit, ir_imm(4), ir_imm(3), ir_imm(1), ir_imm(2)));
    })

    TEST("select less than", 1, {
        ir_retval(jit, ir_select_lt(jit, ir_imm(3), ir_imm(4), ir_imm(1), ir_imm(2)));
    })

    TEST("select not less than", 2, {
        ir_retval(jit, ir_select_lt(jit, ir_imm(3), ir_imm(3), ir_imm(1), ir_imm(2)));
    })

    #undef TEST
    printf("Backend tests done\n");
}

void test_backend_performance()
{
    printf("Running backend performance tests\n");

    codeblock_t codeblock;
    codeblock_t* cb = &codeblock;

    uint8_t* mem_block = alloc_exec_mem(20 * 1024 * 1024);
    cb_init(cb, mem_block, 20 * 1024 * 1024);
    cb_set_pos(cb, 0);

    jitstate_t jitstate;
    jitstate_t* jit = &jitstate;

    #define TEST(NAME, EXPECTED, BODY) \
        jitstate = (jitstate_t){ 0, 0, 0 }; \
        BODY \
        ir_alloc_regs(jit); \
        ir_peephole_opt(jit); \
        ir_gen_x86(cb, jit); \
        ir_jit_clear(jit);

    clock_t start_ticks = clock();

    // Time to 5MB
    while (cb->write_pos < 10 * 1024 * 1024)
    {
        TEST("and", 4, {
            ir_opnd_t opnd0 = ir_add(jit, ir_imm(2), ir_imm(3));
            ir_opnd_t opnd1 = ir_sub(jit, ir_imm(18), ir_imm(4));
            ir_retval(jit, ir_and(jit, opnd0, opnd1));
        });

        TEST("jump equal", 2, {
            ir_opnd_t label = ir_label_opnd((char *) "label");
            ir_jump_eq(jit, ir_imm(3), ir_imm(3), label);
            ir_retval(jit, ir_imm(1));
            ir_label(jit, label);
            ir_retval(jit, ir_imm(2));
        });

        TEST("jump not equal", 2, {
            ir_opnd_t label = ir_label_opnd((char *) "label");
            ir_jump_ne(jit, ir_imm(3), ir_imm(4), label);
            ir_retval(jit, ir_imm(1));
            ir_label(jit, label);
            ir_retval(jit, ir_imm(2));
        });

        TEST("jump overflow", 2, {
            ir_opnd_t label = ir_label_opnd((char *) "label");
            ir_add(jit, ir_imm(INT64_MAX), ir_imm(1));
            ir_jump_ovf(jit, label);
            ir_retval(jit, ir_imm(1));
            ir_label(jit, label);
            ir_retval(jit, ir_imm(2));
        })

        TEST("call plain", 7, {
            ir_retval(jit, ir_ccall(jit, ir_const_ptr((void *)&ir_test_function_plain)));
        });

        TEST("call with arguments", 7, {
            ir_retval(jit, ir_ccall(jit, ir_const_ptr((void *)&ir_test_function_add), ir_imm(3), ir_imm(4)));
        });
    }

    #undef TEST

    clock_t end_ticks = clock();
    clock_t delta_ticks = end_ticks - start_ticks;
    float delta_seconds = (float)delta_ticks / CLOCKS_PER_SEC;

    printf("Backend performance tests done\n");
    printf("It took %lu clicks (%f seconds).\n", delta_ticks, delta_seconds);
}
