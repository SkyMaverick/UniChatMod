#pragma once

int osal_timer_init (uintptr_t handle);
int osal_timer_start (uintptr_t handle, uv_timer_cb cb, uint64_t timeout, uint64_t repeat);
int osal_timer_stop (uintptr_t handle);
int osal_timer_again (uintptr_t handle);
void osal_timer_set_repeat (uintptr_t handle, uint64_t repeat);
uint64_t osal_timer_get_repeat (const uintptr_t handle);
