#include "mainloop.h"

#include "api.h"
#include "config.h"
#include "defs.h"
#include "gettext.h"
#include "mqueue.h"
#include "ucm.h"

#include <stdlib.h>
#include <string.h>

static struct mq_block_s* messages;

int
ucm_mloop_init(int size)
{
    if (size < UCM_DEF_MQ_LIMIT) {
        ucm_etrace("%s %d. %s: %d. %s\n", _("Message queue size don't less minimal size"),
                   UCM_DEF_MQ_LIMIT, _("Now define size"), size, _("Using default value"));
        size = UCM_DEF_MQ_LIMIT;
    }
    messages = mq_create(size);
    return messages ? UCM_RET_SUCCESS : UCM_RET_NOOBJECT;
}

int
ucm_mloop_push(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2)
{
    return mq_push(messages, id, ctx, x1, x2);
}

int
ucm_mloop_pop(uint32_t* id, uintptr_t* ctx, uint32_t* x1, uint32_t* x2)
{
    return mq_pop(messages, id, ctx, x1, x2);
}

void
ucm_mloop_clear(void)
{
    mq_clear(messages);
}

void
ucm_mloop_wait(void)
{
    mq_wait(messages);
}

int
ucm_mloop_noempty(void)
{
    return mq_noempty(messages);
}

void
ucm_mloop_free(void)
{
    if (messages)
        mq_free(messages);
}

static inline size_t
signal_size_get(uint32_t id)
{
    switch (id) {
    case UCM_SIG_START_GUI:
    case UCM_SIG_START_GUI2:
        return sizeof(ucm_sigui_t);
    default:
        return 0;
    }
}

ucm_signal_t*
ucm_signal_alloc2(uint32_t id, void* ctx, size_t mem)
{
    ucm_signal_t* sig = NULL;

    size_t size = signal_size_get(id);
    if (size) {
        ucm_dtrace("%s: %zu. %s: %d\n", "signal alloc", id, "Allocated", size);
        sig = UniAPI->sys.zmalloc(size + mem);
        if (sig) {
            // Define signal arguments
            sig->sig    = id;
            sig->size   = size + mem;
            sig->sender = NULL;

            // copy context
            if (ctx) {
                sig->ctx = sig+ size;
                memcpy(sig->ctx, ctx, mem);
            }
        }
    }
    return sig;
}

ucm_signal_t*
ucm_signal_alloc(uint32_t id)
{
    return ucm_signal_alloc2(id, NULL, 0);
}

void
ucm_signal_free(ucm_signal_t** signal)
{
    if (*signal) {
        ucm_dtrace("Free signal: %d\n", (*signal)->sig);
        switch ((*signal)->sig) {
            // TODO
        }
        ucm_free_null(*signal);
    }
}

int
ucm_signal_push(ucm_signal_t* signal, uint32_t x1, uint32_t x2, void* sender)
{
    if (!signal)
        return UCM_RET_NOOBJECT;

    signal->sender = sender;
    return mq_push(messages, signal->sig, (uintptr_t)signal, x1, x2);
}
