#pragma once

#include "ucm.h"

#include "osal-dynlib.h"
#include "osal-fs.h"
#include "osal-memory.h"
#include "osal-system.h"
#include "osal-threading.h"

typedef struct {
    uv_loop_t* loop_system;
    uv_loop_t* loop_network;
} osal_handler_t;

extern osal_handler_t* o_krnl;
