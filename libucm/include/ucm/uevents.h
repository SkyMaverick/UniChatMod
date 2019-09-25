#pragma once

enum
{
    UCM_EVENT_TERM           = 0x00000000,
    UCM_EVENT_RUN            = 0x00000001,
    UCM_EVENT_INFO           = 0x00000002,
    UCM_EVENT_LOAD_SUCCESS   = 0x00000003,
    UCM_EVENT_DBLOAD_SUCCESS = 0x00000004,
    UCM_EVENT_PLUGS_SUCCESS  = 0x00000005,
    // events with allocated memory
    UCM_EVENT_START_GUI  = 0x80000000,
    UCM_EVENT_START_GUI2 = 0x80000001,
};
#define EVENT_ALLOCATED(X) ((X) >> 31) & 1

typedef struct
{
    ucm_object_t oid;

    uint32_t ev;
    size_t size;
    void* sender;
    void* ctx;
} ucm_ev_t;
#define U_EVENT(X) ((ucm_ev_t*)(X))

typedef struct
{
    ucm_ev_t base;
    char pid[UCM_PID_MAX + 1];
    // TODO
} ucm_evgui_t;
#define U_EVENT_GUI(X) ((ucm_evgui_t*)(X))
