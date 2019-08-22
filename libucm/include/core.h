#pragma once
#include "ucm.h"

extern ucm_plugin_t* ucm_core;

uv_loop_t* get_handle_mainloop(void);

uv_loop_t* get_handle_netloop(void);

void wait_core_loop(void);

UCM_RET
core_load(void);

UCM_RET
core_unload(void);
