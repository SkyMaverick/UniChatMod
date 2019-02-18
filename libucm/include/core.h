#pragma once
#include "ucm.h"

extern ucm_plugin_t* ucm_core;

const uv_loop_t*
get_handle_mainloop (void);

const uv_loop_t*
get_handle_netloop (void);
