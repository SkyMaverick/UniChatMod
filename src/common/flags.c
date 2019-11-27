#include "flags.h"

static uint32_t global_flags = 0;

bool
get_flag(const app_flag_t flag) {
    return (global_flags & flag);
}
void
set_flag(const app_flag_t flag) {
    global_flags |= flag;
}
void
unset_flag(const app_flag_t flag) {
    global_flags &= ~flag;
}
