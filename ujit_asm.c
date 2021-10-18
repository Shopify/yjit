#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

// For mmapp()
#include <sys/mman.h>

#include "ujit_asm.h"

// TODO: give ujit_examples.h some more meaningful file name
#include "ujit_examples.h"

void cb_init(codeblock_t* cb, size_t mem_size)
{
    // Map the memory as executable
    cb->mem_block = (uint8_t*)mmap(
        NULL,
        mem_size,
        PROT_READ | PROT_WRITE | PROT_EXEC,
        MAP_PRIVATE | MAP_ANON,
        -1,
        0
    );

    // Check that the memory mapping was successful
    if (cb->mem_block == MAP_FAILED)
    {
        fprintf(stderr, "mmap call failed\n");
        exit(-1);
    }

    cb->mem_size = mem_size;
    cb->write_pos = 0;
    cb->num_labels = 0;
    cb->num_refs = 0;
}

// Get a direct pointer into the executable memory block
uint8_t* cb_get_ptr(codeblock_t* cb, size_t index)
{
    assert (index < cb->mem_size);
    return &cb->mem_block[index];
}

// Write a byte at the current position
void cb_write_byte(codeblock_t* cb, uint8_t byte)
{
    assert (cb->mem_block);
    assert (cb->write_pos + 1 <= cb->mem_size);
    cb->mem_block[cb->write_pos++] = byte;
}

// Write multiple bytes starting from the current position
void cb_write_bytes(codeblock_t* cb, size_t num_bytes, ...)
{
    va_list va;
    va_start(va, num_bytes);

    for (size_t i = 0; i < num_bytes; ++i)
    {
        uint8_t byte = va_arg(va, int);
        cb_write_byte(cb, byte);
    }

    va_end(va);
}

// Write a signed integer over a given number of bits at the current position
void cb_write_int(codeblock_t* cb, uint64_t val, size_t num_bits)
{
    assert (num_bits > 0);
    assert (num_bits % 8 == 0);

    // Switch on the number of bits
    switch (num_bits)
    {
        case 8:
        cb_write_byte(cb, (uint8_t)val);
        break;

        case 16:
        cb_write_bytes(
            cb,
            2,
            (uint8_t)((val >> 0) & 0xFF),
            (uint8_t)((val >> 8) & 0xFF)
        );
        break;

        case 32:
        cb_write_bytes(
            cb,
            4,
            (uint8_t)((val >>  0) & 0xFF),
            (uint8_t)((val >>  8) & 0xFF),
            (uint8_t)((val >> 16) & 0xFF),
            (uint8_t)((val >> 24) & 0xFF)
        );
        break;

        default:
        {
            // Compute the size in bytes
            size_t num_bytes = num_bits / 8;

            // Write out the bytes
            for (size_t i = 0; i < num_bytes; ++i)
            {
                uint8_t byte_val = (uint8_t)(val & 0xFF);
                cb_write_byte(cb, byte_val);
                val >>= 8;
            }
        }
    }
}

// Ruby instruction prologue and epilogue functions
void cb_write_prologue(codeblock_t* cb)
{
    for (size_t i = 0; i < sizeof(ujit_pre_call_bytes); ++i)
        cb_write_byte(cb, ujit_pre_call_bytes[i]);
}

void cb_write_epilogue(codeblock_t* cb)
{
    for (size_t i = 0; i < sizeof(ujit_post_call_bytes); ++i)
        cb_write_byte(cb, ujit_post_call_bytes[i]);
}

// Write the REX byte
void writeREX(
    codeblock_t* cb,
    bool w_flag,
    uint8_t reg_no,
    uint8_t idx_reg_no,
    uint8_t rm_reg_no
)
{
    // 0 1 0 0 w r x b
    // w - 64-bit operand size flag
    // r - MODRM.reg extension
    // x - SIB.index extension
    // b - MODRM.rm or SIB.base extension
    uint8_t w = w_flag? 1:0;
    uint8_t r = (reg_no & 8)? 1:0;
    uint8_t x = (idx_reg_no & 8)? 1:0;
    uint8_t b = (rm_reg_no & 8)? 1:0;

    // Encode and write the REX byte
    uint8_t rexByte = 0x40 + (w << 3) + (r << 2) + (x << 1) + (b);
    cb_write_byte(cb, rexByte);
}

// Write an opcode byte with an embedded register operand
/*static void cb_write_opcode(codeblock_t* cb, uint8_t opcode, X86Reg rOpnd)
{
    // Write the reg field into the opcode byte
    uint8_t op_byte = opcode | (rOpnd.regNo & 7);
    cb_write_byte(cb, op_byte);
}
*/

// nop - Noop, one or multiple bytes long
void nop(codeblock_t* cb, size_t length)
{
    switch (length)
    {
        case 0:
        break;

        case 1:
        //cb.writeASM("nop1");
        cb_write_byte(cb, 0x90);
        break;

        case 2:
        //cb.writeASM("nop2");
        cb_write_bytes(cb, 2, 0x66,0x90);
        break;

        case 3:
        //cb.writeASM("nop3");
        cb_write_bytes(cb, 3, 0x0F,0x1F,0x00);
        break;

        case 4:
        //cb.writeASM("nop4");
        cb_write_bytes(cb, 4, 0x0F,0x1F,0x40,0x00);
        break;

        case 5:
        //cb.writeASM("nop5");
        cb_write_bytes(cb, 5, 0x0F,0x1F,0x44,0x00,0x00);
        break;

        case 6:
        //cb.writeASM("nop6");
        cb_write_bytes(cb, 6, 0x66,0x0F,0x1F,0x44,0x00,0x00);
        break;

        case 7:
        //cb.writeASM("nop7");
        cb_write_bytes(cb, 7, 0x0F,0x1F,0x80,0x00,0x00,0x00,0x00);
        break;

        case 8:
        //cb.writeASM("nop8");
        cb_write_bytes(cb, 8, 0x0F,0x1F,0x84,0x00,0x00,0x00,0x00,0x00);
        break;

        case 9:
        //cb.writeASM("nop9");
        cb_write_bytes(cb, 9, 0x66,0x0F,0x1F,0x84,0x00,0x00,0x00,0x00,0x00);
        break;

        default:
        {
            size_t written = 0;
            while (written + 9 <= length)
            {
                nop(cb, 9);
                written += 9;
            }
            nop(cb, length - written);
        }
        break;
    }
}

/*
/// push - Push a register on the stack
void push(codeblock_t* cb, X86Reg reg)
{
    assert (reg.size is 64, "can only push 64-bit registers");

    //cb.writeASM("push", reg);

    if (reg.rexNeeded)
        cb_write_rex(cb, false, 0, 0, reg.regNo);
    cb_write_byte(cb, 0x50, reg);
}

/// pop - Pop a register off the stack
void pop(codeblock_t* cb, X86Reg reg)
{
    assert (reg.size is 64);

    //cb.writeASM("pop", reg);

    if (reg.rexNeeded)
        cb_write_rex(false, 0, 0, reg.regNo);
    cb_write_byte(cb, 0x58, reg);
}
*/
