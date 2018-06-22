#ifndef _UCM_EVENT_HOOK_H_
#define _UCM_EVENT_HOOK_H_

#include <stddef.h>
#include <inttypes.h>

typedef  void (*cb_hook)(uint32_t    eid,
                         uintptr_t   ev,
                         uint32_t    x1,
                         uint32_t    x2,
                         void*       ctx);
void
hook_event (uint32_t    eid,
            uintptr_t   ev,
            uint32_t    x1,
            uint32_t    x2);

void
hook_event_attach (cb_hook  hook,
                   void*    ctx,
                   uint32_t mask);

void
hook_event_detach (cb_hook hook);

#endif
