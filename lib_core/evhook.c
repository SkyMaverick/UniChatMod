#include "evhook.h"
#include "ucm.h"
#include "alloc.h"
#include "config.h"
#include "threading.h"

#define zmalloc(size) calloc(1, size);



typedef struct _event_hook_s {
    unsigned mask;                      // events selector mask
    void*    ctx;                       // callback context pointer
    cb_hook hook;

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
    rwlock_rlock (_lock_hooks);
    for (ucm_evhook_t* i = _hooks; i; i=i->next) {
        if ( i->mask | eid) {
            i->hook(eid, ev, x1, x2, i->ctx);
        }
    }
    rwlock_unlock (_lock_hooks);
}

void
hook_event (uint32_t    eid,
            uintptr_t   ev,
            uint32_t    x1,
            uint32_t    x2)
{
    // TODO block some events if it's need
    _hooks_core (eid, ev, x1, x2);
}

void
hook_event_attach (cb_hook hook, void* ctx, uint32_t mask)
{
    ucm_evhook_t* eh = ucm_zmalloc(sizeof(ucm_evhook_t));
    if (eh) {
        eh->hook = hook;
        eh->ctx  = ctx;
        eh->mask = mask;

        rwlock_wlock (_lock_hooks);
        eh->next = _hooks;
        _hooks   = eh;
        rwlock_unlock (_lock_hooks);
    }
}

void
hook_event_detach (cb_hook hook)
{
    ucm_evhook_t* prev = NULL;

    rwlock_wlock(_lock_hooks);
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
    rwlock_unlock(_lock_hooks);
}
