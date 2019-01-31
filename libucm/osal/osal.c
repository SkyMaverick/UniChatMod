#include "osal.h"

void
osal_init (void)
{
    uv_replace_allocator (osal_malloc,
                          osal_realloc,
                          osal_calloc,
                          osal_free);
    // TODO remove this
    p_libsys_init();
}

void
osal_release (void)
{
    // TODO remove this
    p_libsys_shutdown();
}
