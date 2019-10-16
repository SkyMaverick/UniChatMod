#pragma once

#include <inttypes.h>

int
get_system_flag(uint64_t code);
void
set_system_flag(uint64_t code);
void
unset_system_flag(uint64_t code);
