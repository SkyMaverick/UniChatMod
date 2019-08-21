#pragma once

#include "ucm.h"

#include "osal-uvwrap.h"
#include "osal-dynlib.h"
#include "osal-fs.h"
#include "osal-memory.h"
#include "osal-threading.h"
#include "osal-timers.h"

enum {
    OSAL_LOOP_SYSTEM  = 0,
    OSAL_LOOP_NETWORK = 1
};

typedef struct {
    uv_loop_t* loop_system;
    uv_loop_t* loop_network;
} osal_handler_t;

extern osal_handler_t* o_krnl;

int
osal_run (int         loop,
          uv_run_mode mode);
int
osal_errno (void);
uintptr_t
osal_init (void);
int
osal_release (void);

#define LOOP_KRNL   o_krnl->loop_system
#define LOOP_NET    o_krnl->loop_network

