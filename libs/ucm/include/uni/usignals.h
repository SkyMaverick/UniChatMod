#pragma once

enum {
    UCM_SIG_TERM = 0x00000000,
    UCM_SIG_RUN = 0x00000001,
    UCM_SIG_INFO = 0x00000002,
    UCM_SIG_LOAD_SUCCESS = 0x00000003,
    UCM_SIG_DBLOAD_SUCCESS = 0x00000004,
    UCM_SIG_PLUGS_SUCCESS = 0x00000005,
    // events with allocated memory
    UCM_SIG_START_GUI = 0x80000000,
    UCM_SIG_START_GUI2 = 0x80000001,
};
#define SIGNAL_ALLOCATED(X) ((X) >> 31) & 1

typedef struct {
    uint32_t sig;
    size_t size;
    void* sender;
    void* ctx;
} ucm_signal_t;
#define U_SIGNAL(X) ((ucm_signal_t*)(X))

typedef struct {
    ucm_signal_t base;
    char pid[UCM_PID_MAX + 1];
    // TODO
} ucm_sigui_t;
#define U_SIGNAL_GUI(X) ((ucm_sigui_t*)(X))
