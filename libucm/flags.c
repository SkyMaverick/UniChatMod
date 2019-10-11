#include "flags.h"
#include <stddef.h>

typedef struct
{
    uint64_t v1;
} flags_t;

static flags_t flags = { 0 };

int
get_system_flag(uint64_t code)
{
    return (flags.v1 & ((uint64_t)1 << code));
}

void
set_system_flag(uint64_t code)
{
    flags.v1 |= ((uint64_t)1 << code);
}

void
unset_system_flag(uint64_t code)
{
    flags.v1 &= ~((uint64_t)1 << code);
}

int
unget_system_flag(uint64_t code)
{
    int ret = get_system_flag(code);
    unset_system_flag(code);
    return ret;
}
