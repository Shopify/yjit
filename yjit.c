// YJIT combined compilation unit. This setup allows spreading functions
// across different files without having to worry about putting things
// in headers and prefixing function names.
#include "internal.h"
#include "vm_core.h"
#include "vm_callinfo.h"
#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"
#include "vm_sync.h"
#include "yjit.h"

#include "yjit_asm.c"

#ifndef YJIT_CHECK_MODE
# define YJIT_CHECK_MODE 0
#endif

// >= 1: print when output code invalidation happens
// >= 2: dump list of instructions when regions compile
#ifndef YJIT_DUMP_MODE
# define YJIT_DUMP_MODE 0
#endif

#ifdef _WIN32
# define PLATFORM_SUPPORTED_P 0
#else
# define PLATFORM_SUPPORTED_P 1
#endif

// USE_MJIT comes from configure options
#define JIT_ENABLED USE_MJIT

// Code block into which we write machine code
static codeblock_t block;
static codeblock_t *cb = NULL;

// Code block into which we write out-of-line machine code
static codeblock_t outline_block;
static codeblock_t *ocb = NULL;

#if YJIT_STATS
// Comments for generated code
struct yjit_comment {
    uint32_t offset;
    const char *comment;
};

typedef rb_darray(struct yjit_comment) yjit_comment_array_t;
static yjit_comment_array_t yjit_code_comments;

// Counters for generated code
#define YJIT_DECLARE_COUNTERS(...) struct rb_yjit_runtime_counters { \
    int64_t __VA_ARGS__; \
}; \
static char yjit_counter_names[] = #__VA_ARGS__;

YJIT_DECLARE_COUNTERS(
    exec_instruction,

    send_keywords,
    send_kw_splat,
    send_args_splat,
    send_block_arg,
    send_ivar_set_method,
    send_zsuper_method,
    send_undef_method,
    send_optimized_method,
    send_missing_method,
    send_bmethod,
    send_refined_method,
    send_cfunc_ruby_array_varg,
    send_cfunc_argc_mismatch,
    send_cfunc_toomany_args,
    send_cfunc_tracing,
    send_iseq_tailcall,
    send_iseq_arity_error,
    send_iseq_only_keywords,
    send_iseq_complex_callee,
    send_not_implemented_method,
    send_getter_arity,
    send_se_cf_overflow,
    send_se_protected_check_failed,

    traced_cfunc_return,

    invokesuper_me_changed,
    invokesuper_block,

    leave_se_interrupt,
    leave_interp_return,
    leave_start_pc_non_zero,

    getivar_se_self_not_heap,
    getivar_idx_out_of_range,

    setivar_se_self_not_heap,
    setivar_idx_out_of_range,
    setivar_val_heapobject,
    setivar_name_not_mapped,
    setivar_not_object,
    setivar_frozen,

    oaref_argc_not_one,
    oaref_arg_not_fixnum,

    opt_getinlinecache_miss,

    binding_allocations,
    binding_set,

    vm_insns_count,
    compiled_iseq_count,
    compiled_block_count,

    invalidation_count,
    invalidate_method_lookup,
    invalidate_bop_redefined,
    invalidate_ractor_spawn,
    invalidate_constant_state_bump,
    invalidate_constant_ic_fill,

    constant_state_bumps,

    expandarray_splat,
    expandarray_postarg,
    expandarray_not_array,
    expandarray_rhs_too_small,

    gbpp_block_param_modified,
    gbpp_block_handler_not_iseq,

    // Member with known name for iterating over counters
    last_member
)

static struct rb_yjit_runtime_counters yjit_runtime_counters = { 0 };
#undef YJIT_DECLARE_COUNTERS

#endif // YJIT_STATS

// The number of bytes counting from the beginning of the inline code block
// that should not be changed. After patching for global invalidation, no one
// should make changes to the invalidated code region anymore. This is used to
// break out of invalidation race when there are multiple ractors.
static uint32_t yjit_codepage_frozen_bytes = 0;

#include "yjit_utils.c"
#include "yjit_core.c"
#include "yjit_iface.c"
#include "yjit_codegen.c"
