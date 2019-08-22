#include "osal-intrnl.h"

uintptr_t osal_timer_create(void)
{
    uv_timer_t* ret = osal_malloc(sizeof(uv_timer_t));
    if (ret) {
        if (uv_timer_init(LOOP_KRNL, ret))
            osal_free_null(ret);
    }
    return (uintptr_t)ret;
}

int osal_timer_start(uintptr_t handle, uv_timer_cb cb, uint64_t timeout, uint64_t repeat)
{
    return uv_timer_start((uv_timer_t*)handle, cb, timeout, repeat);
}

int osal_timer_stop(uintptr_t handle) { return uv_timer_stop((uv_timer_t*)handle); }
int osal_timer_again(uintptr_t handle) { return uv_timer_again((uv_timer_t*)handle); }
void osal_timer_set_repeat(uintptr_t handle, uint64_t repeat) { uv_timer_set_repeat((uv_timer_t*)handle, repeat); }
uint64_t osal_timer_get_repeat(const uintptr_t handle) { return uv_timer_get_repeat((uv_timer_t*)handle); }

void osal_timer_release(uintptr_t handle)
{
    osal_timer_stop(handle);
    osal_free((void*)handle);
}
