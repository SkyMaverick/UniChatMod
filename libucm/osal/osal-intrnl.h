#pragma once

#include <uv.h>

typedef struct {
    uv_loop_t* loop_system;
    uv_loop_t* loop_network;
} osal_handler_t;

extern osal_handler_t* o_krnl;
