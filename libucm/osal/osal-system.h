#pragma once

enum {
    OSAL_LOOP_SYSTEM  = 0,
    OSAL_LOOP_NETWORK = 1
};

int
osal_run (int loop, uv_run_mode mode);

int
osal_errno ();

uintptr_t
osal_init (void);
int
osal_release (void);
