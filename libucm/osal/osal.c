#include "osal-intrnl.h"

int
osal_init (void)
{
       
    // uv_run
    /* Use internal functions for libuv allocator */
    uv_replace_allocator (osal_malloc,
                          osal_realloc,
                          osal_calloc,
                          osal_free);
    // TODO remove this
    p_libsys_init();
}

int
osal_release (void)
{
    // TODO remove this
    p_libsys_shutdown();
}
