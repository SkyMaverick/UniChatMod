#pragma once

#include <inttypes.h>
#include <stddef.h>

typedef void (*cb_evhook)(uint32_t eid, uintptr_t ev, uint32_t x1, uint32_t x2, void* ctx);
void hooks_event_init(void);

void hooks_event_release(void);

void hooks_event(const uint32_t eid, const uintptr_t ev, const uint32_t x1, const uint32_t x2);

void hooks_event_attach(cb_evhook hook, void* ctx, uint32_t mask);

void hooks_event_detach(cb_evhook hook);
