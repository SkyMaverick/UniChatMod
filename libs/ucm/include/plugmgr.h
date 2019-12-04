#pragma once

#include "ucm.h"

size_t
pmgr_load(char* path);

const bool
pmgr_isload(void);

void
pmgr_unload(void);

uintptr_t
pmgr_first(unsigned type);

uintptr_t
pmgr_next(uintptr_t pld);

void
pmgr_close(uintptr_t* pld);

void
pmgr_message_process(const uint32_t id, const uintptr_t ctx, const uint32_t x1, const uint32_t x2);
