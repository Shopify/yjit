#include <assert.h>
#include "insns.inc"
#include "internal.h"
#include "vm_core.h"
#include "vm_callinfo.h"
#include "builtin.h"
#include "insns_info.inc"
#include "ujit_compile.h"
#include "ujit_asm.h"

// TODO: give ujit_examples.h some more meaningful file name
#include "ujit_examples.h"

static codeblock_t block;
static codeblock_t* cb = NULL;

// Hash table of encoded instructions
extern st_table *rb_encoded_insn_data;

static void ujit_init();

// Ruby instruction entry
static void
ujit_instr_entry(codeblock_t* cb)
{
    for (size_t i = 0; i < sizeof(ujit_pre_call_bytes); ++i)
        cb_write_byte(cb, ujit_pre_call_bytes[i]);
}

// Ruby instruction exit
static void
ujit_instr_exit(codeblock_t* cb)
{
    for (size_t i = 0; i < sizeof(ujit_post_call_bytes); ++i)
        cb_write_byte(cb, ujit_post_call_bytes[i]);
}

// Keep track of mapping from instructions to generated code
// See comment for rb_encoded_insn_data in iseq.c
static void
addr2insn_bookkeeping(void *code_ptr, int insn)
{
    const void * const *table = rb_vm_get_insns_address_table();
    const void * const translated_address = table[insn];
    st_data_t encoded_insn_data;
    if (st_lookup(rb_encoded_insn_data, (st_data_t)translated_address, &encoded_insn_data)) {
        st_insert(rb_encoded_insn_data, (st_data_t)code_ptr, encoded_insn_data);
    }
    else {
        rb_bug("ujit: failed to find info for original instruction while dealing with addr2insn");
    }
}

// Generate a chunk of machinecode for one individual bytecode instruction
// Eventually, this will handle multiple instructions in a sequence
uint8_t *
ujit_compile_insn(rb_iseq_t *iseq, size_t insn_idx)
{
    // If not previously done, initialize ujit
    if (!cb)
    {
        ujit_init();
    }

    if (cb->write_pos + 1024 >= cb->mem_size)
    {
        rb_bug("out of executable memory");
    }

    int insn = (int)iseq->body->iseq_encoded[insn_idx];
	int len = insn_len(insn);
    //const char* name = insn_name(insn);
    //printf("%s\n", name);

    // Compute the address of the next instruction
    void* next_pc = &iseq->body->iseq_encoded[insn_idx + len];

    // Get a pointer to the current write position in the code block
    uint8_t* code_ptr = &cb->mem_block[cb->write_pos];
    //printf("write pos: %ld\n", cb->write_pos);

    // Write the pre call bytes
    ujit_instr_entry(cb);

    // TODO: encode individual instructions, eg
    // nop, putnil, putobject, putself, pop, dup, getlocal, setlocal, nilp

    // TODO: we should move the codegen for individual instructions
    // into separate functions
    if (insn == BIN(nop))
    {
        //add(cb, RSI, imm_opnd(8));                  // increment PC
        //mov(cb, mem_opnd(64, RDI, 0), RSI);         // write new PC to EC object, not necessary for nop bytecode?
        //mov(cb, RAX, RSI);                          // return new PC

        // Directly return the next PC, which is a constant
        mov(cb, RAX, const_ptr_opnd(next_pc));

        // Write the post call bytes
        ujit_instr_exit(cb);

        addr2insn_bookkeeping(code_ptr, insn);

        return code_ptr;
    }

    if (insn == BIN(pop))
    {
        // Decrement SP
        sub(cb, mem_opnd(64, RDI, 8), imm_opnd(8));

        // Directly return the next PC, which is a constant
        mov(cb, RAX, const_ptr_opnd(next_pc));

        // Write the post call bytes
        ujit_instr_exit(cb);

        addr2insn_bookkeeping(code_ptr, insn);

        return code_ptr;
    }

    if (insn == BIN(putobject_INT2FIX_0_) || insn == BIN(putobject_INT2FIX_1_))
    {
        // Load current SP into RAX
        mov(cb, RAX, mem_opnd(64, RDI, 8));

        // Write constant at SP
        int cst = (insn == BIN(putobject_INT2FIX_0_))? 0:1;
        mov(cb, mem_opnd(64, RAX, 0), imm_opnd(INT2FIX(cst)));

        // Load incremented SP into RCX
        lea(cb, RCX, mem_opnd(64, RAX, 8));

        // Write back incremented SP
        mov(cb, mem_opnd(64, RDI, 8), RCX);

        // Directly return the next PC, which is a constant
        mov(cb, RAX, const_ptr_opnd(next_pc));

        // Write the post call bytes
        ujit_instr_exit(cb);

        addr2insn_bookkeeping(code_ptr, insn);

        return code_ptr;
    }

    return 0;
}

static void ujit_init()
{
    // 4MB ought to be enough for anybody
    cb = &block;
    cb_init(cb, 4000000);
}
