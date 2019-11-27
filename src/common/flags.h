#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef enum {
    FLAG_APP_PORTABLE = 1 << 0,
    FLAG_APP_PORTABLE_BASE = 1 << 1,
    FLAG_APP_TERMINATED = 1 << 2
} app_flag_t;

bool
get_flag(const app_flag_t flag);
void
set_flag(const app_flag_t flag);
void
unset_flag(const app_flag_t flag);
