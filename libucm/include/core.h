#pragma once
#include "ucm.h"

extern ucm_plugin_t* ucm_core;

void
wait_core_loop(void);

UCM_RET
core_load(void);

UCM_RET
core_unload(void);

const uintptr_t
get_loop_handle(int loop);
