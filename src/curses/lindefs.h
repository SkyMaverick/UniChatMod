#pragma once

typedef struct {
    WINDOW* win_main;
    struct {
        int out_tmp;
        int err_tmp;
    } fds;
} npc_window_t;

#define NPC_MEMNEED_WINDOW sizeof(npc_window_t)
