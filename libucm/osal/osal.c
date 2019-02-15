#include "osal-intrnl.h"

static inline bool
_create_loop (uv_loop_t** loop)
{
    *loop = osal_malloc(sizeof(uv_loop_t));
    if (*loop) {
        if (uv_loop_init (*loop) > 0) {
            osal_free_null (*loop);
            goto exit;
        } else {
            return true;
        }
    }
    exit:return false;
}

static inline void
_release_loop (uv_loop_t** loop)
{
    if (*loop) {
        uv_loop_close (*loop);
        osal_free_null (*loop);
    }
}

int
osal_init (void)
{
    /* Use internal functions for libuv allocator */
    uv_replace_allocator (osal_malloc,
                          osal_realloc,
                          osal_calloc,
                          osal_free);
    
   if ( ( _create_loop(&osal_loop_system) ) &&
        ( _create_loop(&osal_loop_fs)     ) &&
        ( _create_loop(&osal_loop_net)    )
      )
   {
        return 0;
   }

   osal_release ();
   return 1;
}

int
osal_release (void)
{
    _release_loop (&osal_loop_system);
    _release_loop (&osal_loop_fs);
    _release_loop (&osal_loop_net);
    
    return 0;
}

const uv_loop_t*
get_system_loop(void)
{
    return osal_loop_system;
}

const uv_loop_t*
get_filesystem_loop(void)
{
    return osal_loop_fs;
}

const uv_loop_t*
get_network_loop(void)
{
    return osal_loop_net;
}
