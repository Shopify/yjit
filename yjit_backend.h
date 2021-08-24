#ifndef YJIT_BACKEND_H
#define YJIT_BACKEND_H 1

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "darray.h"
#include "internal/static_assert.h"
#include "internal.h"

// Operand to an IR instruction
enum yjit_ir_opnd_type
{
    EIR_VOID = 0,   // For insns with no output
    //EIR_STACK,    // Value on the temp stack (idx)
    //EIR_LOCAL,    // Local variable (idx, do we need depth too?)
    EIR_VALUE,      // Immediate Ruby value, may be GC'd, movable
    EIR_INSN_OUT,   // Output of a preceding instruction in this block
    EIR_CODE_PTR,   // Pointer to a piece of code (e.g. side-exit)
    EIR_LABEL_NAME, // A label without an index in the output
    EIR_LABEL_IDX,  // A label that has been indexed

    // Low-level operands, for lowering
    EIR_MEM,        // Memory location (num_bits, base_ptr, const_offset)
    EIR_IMM,        // Raw, non GC'd immediate (num_bits, val)
    EIR_REG         // Machine register (num_bits, idx)
};

// Register value used by IR operands
typedef struct yjit_reg_t
{
    // Register index
    uint8_t idx: 5;

    // Special register flag EC/CFP/SP/SELF
    bool special: 1;

} ir_reg_t;
STATIC_ASSERT(ir_reg_size, sizeof(ir_reg_t) <= 8);

// Operand to an IR instruction
typedef struct yjit_ir_opnd_t
{
    // Payload
    union
    {
        // Memory location
        struct {
            uint32_t disp;

            // Base register
            ir_reg_t base;
        } mem;

        // Register operand
        ir_reg_t reg;

        // Value immediates
        VALUE value;

        // Raw immediates
        int64_t imm;
        uint64_t u_imm;

        // For local/stack/insn
        uint32_t idx;

        // For branch targets
        uint8_t* code_ptr;

        // For strings (names, comments, etc.)
        char* str;
    } as;

    // Size in bits (8, 16, 32, 64)
    uint8_t num_bits;

    // Kind of IR operand
    uint8_t kind: 4;

} ir_opnd_t;
STATIC_ASSERT(ir_opnd_size, sizeof(ir_opnd_t) <= 16);

// TODO: we can later rename these to OPND_SELF, OPND_EC, etc.
//       But currently those names would clash with yjit_core.h
#define IR_VOID ( (ir_opnd_t){ .kind = EIR_VOID } )
#define IR_EC   ( (ir_opnd_t){ .num_bits = 64, .kind = EIR_REG, .as.reg.idx = 0, .as.reg.special = 1 } )
#define IR_CFP  ( (ir_opnd_t){ .num_bits = 64, .kind = EIR_REG, .as.reg.idx = 1, .as.reg.special = 1 } )
#define IR_SP   ( (ir_opnd_t){ .num_bits = 64, .kind = EIR_REG, .as.reg.idx = 2, .as.reg.special = 1 } )
#define IR_SELF ( (ir_opnd_t){ .num_bits = 64, .kind = EIR_REG, .as.reg.idx = 3, .as.reg.special = 1 } )

// Low-level register operand
#define IR_REG(x86reg) ( (ir_opnd_t){ .num_bits = 64, .kind = EIR_REG, .as.reg.idx = x86reg.as.reg.reg_no } )

ir_opnd_t ir_code_ptr(uint8_t* code_ptr);
ir_opnd_t ir_const_ptr(void *ptr);
ir_opnd_t ir_imm(int64_t val);
ir_opnd_t ir_mem(uint8_t num_bits, ir_opnd_t base, int32_t disp);

// Instruction opcodes
enum yjit_ir_op
{
    // Comment strings in the generated code
    OP_COMMENT,

    // Named intra-block label we can jump to
    OP_LABEL,

    // Arithmetic instructions
    OP_ADD,
    OP_SUB,
    OP_AND,
    OP_NOT,

    // For later:
    // These encode Ruby true/false semantics
    // Can be used to enable op fusion of Ruby compare + branch
    // JUMP_TRUE (opnd, target),
    // JUMP_FALSE (opnd, target)

    // For later:
    // GUARD_HEAP (opnd, target)
    // GUARD_IMM (opnd, target)
    // GUARD_FIXNUM (opnd, target)

    // Comparison
    OP_CMP,
    OP_TEST,

    // Conditional jumps
    OP_JUMP_EQ,
    OP_JUMP_NE,
    OP_JUMP_OVF,

    // TODO:
    //CALL_CFUNC (var-arg...)
    OP_RET,
    OP_RETVAL,

    //COUNTER_INC (counter_name)

    // Low-level instructions
    OP_LEA,
    OP_MOV,

    // Upper bound for opcodes
    OP_MAX
};

// Array of operands
typedef rb_darray(ir_opnd_t) opnd_array_t;

// IR instruction
typedef struct yjit_ir_insn_t
{
    // TODO: do may need a union here to store a branch target?
    // How do we want to encore branch targets?
    opnd_array_t opnds;

    // Position in the generated machine code
    // Useful for comments and for patching jumps
    uint32_t pos;

    // Opcode for the instruction
    uint8_t op;

    // TODO: should we store 2-4 operands by default?
    // Some insns, like calls, will need to allow for a list of N operands
    // Most other instructions will have just 2-3
    // Many calls will also have just 1-3
    //bool many_args: 1;

} ir_insn_t;
STATIC_ASSERT(ir_insn_size, sizeof(ir_insn_t) <= 64);

// Array of instruction
typedef rb_darray(ir_insn_t) insn_array_t;

// Test code
void test_backend();

#endif // #ifndef YJIT_BACKEND_H
