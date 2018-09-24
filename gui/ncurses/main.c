#include <curses.h>

#include "ucm.h"
#include "main.h"

extern const ucm_functions_t* core;
static uintptr_t    hwnd = 0;

static void
_curses_cleanup (void)
{
    endwin();
}

static void 
_curses_loop (void* ctx)
{
    ucm_cargs_t* args = (ucm_cargs_t*) ctx;
    fprintf (stdout, "%s\n", "Start curses APP");

    initscr();
    keypad(stdscr, TRUE);
    (void) nonl();
    (void) cbreak();
    (void) noecho();

    if (has_colors())
    {
        start_color();

        init_pair (COLOR_BLACK,   COLOR_BLACK,   COLOR_BLACK);
        init_pair (COLOR_GREEN,   COLOR_GREEN,   COLOR_BLACK);
        init_pair (COLOR_RED,     COLOR_RED,     COLOR_BLACK);
        init_pair (COLOR_CYAN,    COLOR_CYAN,    COLOR_BLACK);
        init_pair (COLOR_WHITE,   COLOR_WHITE,   COLOR_BLACK);
        init_pair (COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK);
        init_pair (COLOR_BLUE,    COLOR_BLUE,    COLOR_BLACK);
        init_pair (COLOR_YELLOW,  COLOR_YELLOW,  COLOR_BLACK);
    }

    for (;;)
    {
        int c = getch();
        if (c == 'q')
            break;
    }

    _curses_cleanup();
}

// *********************************************************
//      EXTERNAL FUNCTIONS
// *********************************************************

UCM_RET
start_curses_app (void* ctx)
{
    hwnd = core->thread_create (_curses_loop, ctx);
    core->thread_join (hwnd);

    return UCM_RET_SUCCESS;
}

void
finish_curses_app (int sig)
{
    UNUSED (sig);
    // TODO
}
