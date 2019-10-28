#include "../app.h"

#include <curses.h>
#include <panel.h>

static npc_window_t* win_main;

static UCM_RET
_curses_init(void)
{
    int ret_status = UCM_RET_SUCCESS;

    win_main = ucm_api->sys.zmalloc(NPC_MEMNEED_WINDOW);
    if (win_main) {
        initscr();
        keypad(stdscr, TRUE);
        (void)nonl();
        (void)cbreak();
        (void)noecho();

        if (has_colors()) {
            start_color();

            init_pair(1, COLOR_BLACK, COLOR_BLACK);
            init_pair(2, COLOR_GREEN, COLOR_BLACK);
            init_pair(3, COLOR_RED, COLOR_BLACK);
            init_pair(4, COLOR_CYAN, COLOR_BLACK);
            init_pair(5, COLOR_WHITE, COLOR_BLACK);
            init_pair(6, COLOR_MAGENTA, COLOR_BLACK);
            init_pair(7, COLOR_BLUE, COLOR_BLACK);
            init_pair(8, COLOR_YELLOW, COLOR_BLACK);
        }
        refresh();
    } else {
        ret_status = UCM_RET_SYSTEM_NOMEMORY;
    }

    return ret_status;
}

static void*
_curses_loop(void* ctx)
{
    int ch = 0;
    // TODO Temporary F1 - it's exit
    while ((ch = getch()) != KEY_F(1) || get_flag(FLAG_APP_TERMINATED)) {
        addch(ch);
    }
    if (!get_flag(FLAG_APP_TERMINATED))
        ucm_api->app.mainloop_msg_send(UCM_SIG_TERM, 0, 0, 0);

    return NULL;
}

static void
_curses_finish(void)
{
    endwin();

    if (win_main)
        free_and_null(win_main);
}

UCM_RET
curses_start(void)
{
    _curses_init();

    ucm_api->sys.thread_create(_curses_loop, NULL);

    erase();
}

void
curses_finish(void)
{
    _curses_finish();
}
