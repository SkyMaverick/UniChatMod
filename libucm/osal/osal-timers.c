#include "osal-intrnl.h"

int osal_timer_init (uintptr_t handle) {
    return uv_timer_init (LOOP_KRNL, (uv_timer_t*)handle);
}
int osal_timer_start (uintptr_t handle, uv_timer_cb cb, uint64_t timeout, uint64_t repeat) {
    return uv_timer_start ((uv_timer_t*) handle, cb, timeout, repeat);
}
int osal_timer_stop (uintptr_t handle) {
    return uv_timer_stop ((uv_timer_t*) handle);
}
int osal_timer_again (uintptr_t handle) {
    return uv_timer_again ((uv_timer_t*) handle);
}
void osal_timer_set_repeat (uintptr_t handle, uint64_t repeat) {
    return uv_timer_set_repeat ((uv_timer_t*) handle, repeat);
}
uint64_t osal_timer_get_repeat (const uintptr_t handle) {
    return  uv_timer_get_repeat ((uv_timer_t*) handle);
}
