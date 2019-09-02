#pragma once

#define TUI_APP_NAME "ucmc"

#include "config.h"
#include "ucm.h"

#include <stdbool.h>

typedef enum
{
    FLAG_APP_PORTABLE      = 1 << 0,
    FLAG_APP_PORTABLE_BASE = 1 << 1,
    FLAG_APP_TERMINATED    = 1 << 2
} app_flag_t;

extern const ucm_functions_t* core;

extern ucm_cstart_func core_start;
extern ucm_cstop_func core_stop;
extern ucm_cinfo_func core_info;

const bool
get_flag(const app_flag_t flag);
void
set_flag(const app_flag_t flag);
void
unset_flag(const app_flag_t flag);
