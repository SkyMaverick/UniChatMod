#include "osal-intrnl.h"

static osal_handler_t osal_kernel = {.loop_system = NULL, .loop_network = NULL};

osal_handler_t* o_krnl = &osal_kernel;

int osal_run(int loop, uv_run_mode mode)
{
    switch (loop) {
    case OSAL_LOOP_SYSTEM:
        return uv_run(o_krnl->loop_system, mode);
    case OSAL_LOOP_NETWORK:
        return uv_run(o_krnl->loop_network, mode);
    default:
        return -1;
    }
}

int osal_errno(void)
{
#if defined(UCM_OS_WINDOWS)
    return GetLastError();
#else
    return errno;
#endif
}

uintptr_t osal_init(void)
{
    o_krnl->loop_system = uv_default_loop();
    if (o_krnl->loop_system == NULL) {
        return 0;
    }

    o_krnl->loop_network = osal_zmalloc(sizeof(uv_loop_t));
    if (o_krnl->loop_network == NULL) {
        osal_release();
        return 0;
    }

    uv_loop_init(o_krnl->loop_network);

    uv_run(o_krnl->loop_system, UV_RUN_DEFAULT);
    uv_run(o_krnl->loop_network, UV_RUN_DEFAULT);

    return (uintptr_t)o_krnl;
}

int osal_release(void)
{
    uv_loop_close(o_krnl->loop_system);
    uv_loop_close(o_krnl->loop_network);
    osal_free_null(o_krnl->loop_network);

    return 0;
}
