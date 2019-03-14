#include "osal-intrnl.h"

int
osal_run (int loop, uv_run_mode mode)
{
    switch (loop) {
        case OSAL_LOOP_SYSTEM:
            return uv_run (o_krnl->loop_system, mode);
        case OSAL_LOOP_NETWORK:
            return uv_run (o_krnl->loop_network, mode);
        default:
            return -1;
    }
}

int
osal_errno ()
{
#if defined (UCM_OS_WINDOWS)
    return GetLastError();
#else
    return errno;
#endif
}

uintptr_t
osal_init (void)
{
    o_krnl->loop_system = osal_zmalloc (sizeof(uv_loop_t));
    if (o_krnl->loop_system == NULL) {
        return 0;
    }

    o_krnl->loop_network = osal_zmalloc (sizeof(uv_loop_t));
    if (o_krnl->loop_network == NULL) {
        osal_free_null (o_krnl->loop_system);
        return 0;
    }
    
    uv_loop_init (o_krnl->loop_system);
    uv_loop_init (o_krnl->loop_network);

    return (uintptr_t)o_krnl;
}

int
osal_release (void)
{
    uv_loop_close   (o_krnl->loop_system);
    osal_free_null  (o_krnl->loop_system);
    uv_loop_close   (o_krnl->loop_network);
    osal_free_null  (o_krnl->loop_network);

    return 0;
}
