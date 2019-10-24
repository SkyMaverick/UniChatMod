#include "../app.h"

#include <curses.h>
#include <panel.h>

static void
_curses_init (void)
{
    initscr();
    keypad(stdscr, TRUE);
    (void)nonl();
    (void)cbreak();
    (void)noecho();

    if (has_colors()) {
        start_color();

        init_pair(1 , COLOR_BLACK   , COLOR_BLACK);
        init_pair(2 , COLOR_GREEN   , COLOR_BLACK);
        init_pair(3 , COLOR_RED     , COLOR_BLACK);
        init_pair(4 , COLOR_CYAN    , COLOR_BLACK);
        init_pair(5 , COLOR_WHITE   , COLOR_BLACK);
        init_pair(6 , COLOR_MAGENTA , COLOR_BLACK);
        init_pair(7 , COLOR_BLUE    , COLOR_BLACK);
        init_pair(8 , COLOR_YELLOW  , COLOR_BLACK);
    }
    refresh();
}

static void
_curses_loop (void)
{
    int ch = 0;
    while ((ch = getch()) != KEY_F(1)) {
        addch(ch);
    }
    ucm_api->app.mainloop_msg_send (UCM_SIG_TERM, 0, 0, 0);
}

static void
_curses_finish (void)
{
    endwin();
}




UCM_RET
curses_start(void)
{
    _curses_init();
    _curses_loop();
}

void
curses_finish(void) {
    _curses_finish();
}
