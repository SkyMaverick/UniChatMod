#pragma once

#include "ucm.h"

#include "osal-dynlib.h"
#include "osal-fs.h"
#include "osal-memory.h"
#include "osal-system.h"
#include "osal-threading.h"
#include "osal-timers.h"

typedef struct {
    uv_loop_t* loop_system;
    uv_loop_t* loop_network;
} osal_handler_t;

extern osal_handler_t* o_krnl;

#define LOOP_KRNL   o_krnl->loop_system
#define LOOP_NET    o_krnl->loop_network
