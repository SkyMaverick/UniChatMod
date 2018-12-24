#include "evhook.h"
#include "ucm.h"
#include "api.h"
#include "config.h"
#include "osal.h"

typedef struct _event_hook_s {
    unsigned  mask;                      // events selector mask
    void*     ctx;                       // callback context pointer
    cb_evhook hook;

    struct _event_hook_s* next;
} ucm_evhook_t;

static ucm_evhook_t* _hooks;
static uintptr_t _lock_hooks;

static void
_hooks_core (uint32_t    eid,
             uintptr_t   ev,
             uint32_t    x1,
             uint32_t    x2)
{
    ucm_rwlock_rlock (_lock_hooks);
    for (ucm_evhook_t* i = _hooks; i; i=i->next) {
        if ( i->mask | eid) {
            i->hook(eid, ev, x1, x2, i->ctx);
        }
    }
    ucm_rwlock_unlock (_lock_hooks);
}

static void
_hooks_flush (ucm_evhook_t** list) 
{
    ucm_evhook_t* tmp = NULL;
    while (*list) {
        tmp = *list;
        *list = (*list)->next;
        ucm_free_null(tmp);
    }
}

void
hooks_event_init (void)
{
    _hooks = NULL;
    _lock_hooks = ucm_rwlock_create();
}

void
hooks_event_release (void)
{
    _hooks_flush(&_hooks);
    ucm_rwlock_free(_lock_hooks);
}

void
hooks_event (const uint32_t    eid,
             const uintptr_t   ev,
             const uint32_t    x1,
             const uint32_t    x2)
{
    // TODO block some events if it's need
    _hooks_core (eid, ev, x1, x2);
}

void
hooks_event_attach (cb_evhook  hook, 
                    void*      ctx, 
                    uint32_t   mask)
{
    ucm_evhook_t* eh = ucm_zmalloc(sizeof(ucm_evhook_t));
    if (eh) {
        eh->hook = hook;
        eh->ctx  = ctx;
        eh->mask = mask;

        ucm_rwlock_wlock (_lock_hooks);
        eh->next = _hooks;
        _hooks   = eh;
        ucm_rwlock_unlock (_lock_hooks);
    }
}

void
hooks_event_detach (cb_evhook hook)
{
    ucm_evhook_t* prev = NULL;

    ucm_rwlock_wlock(_lock_hooks);
    for (ucm_evhook_t* ev = _hooks; ev; prev = ev, ev=ev->next) {
        if ( ev->hook == hook ) {
            if (prev) {
                prev->next = ev->next;
            } else {
                _hooks = ev->next;
            }
            ucm_free (ev);
            break;
        }
    }
    ucm_rwlock_unlock(_lock_hooks);
}
