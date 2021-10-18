//
// This file contains definitions uJIT exposes to the CRuby codebase
//

#ifndef UJIT_H
#define UJIT_H 1

#include "stddef.h"
#include "stdint.h"
#include "stdbool.h"
#include "method.h"

#ifdef _WIN32
#define PLATFORM_SUPPORTED_P 0
#else
#define PLATFORM_SUPPORTED_P 1
#endif

#ifndef UJIT_CHECK_MODE
#define UJIT_CHECK_MODE 0
#endif

// >= 1: print when output code invalidation happens
// >= 2: dump list of instructions when regions compile
#ifndef UJIT_DUMP_MODE
#define UJIT_DUMP_MODE 0
#endif

#ifndef rb_iseq_t
typedef struct rb_iseq_struct rb_iseq_t;
#define rb_iseq_t rb_iseq_t
#endif

struct rb_ujit_options {
    bool ujit_enabled;

    // Number of method calls after which to start generating code
    // Threshold==1 means compile on first execution
    unsigned call_threshold;

    // Capture and print out stats
    bool gen_stats;
};

RUBY_SYMBOL_EXPORT_BEGIN
bool rb_ujit_enabled_p(void);
unsigned rb_ujit_call_threshold(void);
RUBY_SYMBOL_EXPORT_END

void rb_ujit_collect_vm_usage_insn(int insn);
void rb_ujit_method_lookup_change(VALUE cme_or_cc);
void rb_ujit_compile_iseq(const rb_iseq_t *iseq, rb_execution_context_t *ec);
void rb_ujit_init(struct rb_ujit_options *options);
void rb_ujit_bop_redefined(VALUE klass, const rb_method_entry_t *me, enum ruby_basic_operators bop);
void rb_ujit_constant_state_changed(void);
void rb_ujit_iseq_mark(const struct rb_iseq_constant_body *body);
void rb_ujit_iseq_update_references(const struct rb_iseq_constant_body *body);
void rb_ujit_iseq_free(const struct rb_iseq_constant_body *body);
void rb_ujit_before_ractor_spawn(void);

#endif // #ifndef UJIT_H
