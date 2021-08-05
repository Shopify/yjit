#ifndef JITDUMP
#define JITDUMP 0

#include <stdint.h>
#include <time.h>

/* Based on
 * linux/tools/perf/Documentation/jitdump-specification.txt
 */

#define JITDUMP_MAGIC 0x4A695444
#define JITDUMP_VERSION 1

#define ELF_MACH_X86 3
#define ELF_MACH_X64 62
#define ELF_MACH_ARM 40
#define ELF_MACH_MIPS 10
#define ELF_MACH_ARM64 183

struct jitdump_file_header {
    uint32_t magic;
    uint32_t version;
    uint32_t total_size;
    uint32_t elf_mach;
    uint32_t pad1;
    uint32_t pid;
    uint64_t timestamp;
    uint64_t flags;
};

struct jitdump_record_header {
    uint32_t record_type;
    uint32_t total_size;
    uint64_t timestamp;
};

#define JITDUMP_CODE_LOAD 0

struct jitdump_code_load {
    struct jitdump_record_header p;
    uint32_t pid;
    uint32_t tid;
    uint64_t vma;
    uint64_t code_addr;
    uint64_t code_size;
    uint64_t code_index;
    // char[] function name
    // char[] native code
};

static uint64_t jitdump_timestamp() {
    struct timespec tv;
    clock_gettime(CLOCK_MONOTONIC, &tv);
    return (uint64_t)tv.tv_nsec + (uint64_t)tv.tv_sec * 1000000000L;
}

#endif
