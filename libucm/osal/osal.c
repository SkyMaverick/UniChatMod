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

    osal_uv_loop = osal_malloc (sizeof(uv_loop_t));
    if (osal_uv_loop) {
        uv_loop_init(osal_uv_loop);
        return 0;
    }
    return 1;
}

int
osal_release (void)
{
    uv_loop_close (osal_uv_loop);
    osal_free (osal_uv_loop);
    return 0;
}
