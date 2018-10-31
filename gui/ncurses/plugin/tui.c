#include <curses.h>

#include "ucm.h"
#include "tui.h"
#include "config.h"

const ucm_functions_t* app;

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
    hwnd = app->thread_create (_curses_loop, ctx);
    app->thread_join (hwnd);

    return UCM_RET_SUCCESS;
}

void
finish_curses_app (int sig)
{
    UNUSED (sig);
    // TODO
}


static UCM_RET
_run_plugin (void)
{
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_plugin (void)
{
    return UCM_RET_SUCCESS;
}

static void
_message (uint32_t  id,
          uintptr_t ctx,
          uint32_t  x1,
          uint32_t  x2)
{
    //TODO
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

static ucm_uiplugin_t plugin = {
    .core.oid                = UCM_TYPE_OBJECT_PLUGIN,
    .core.info.api           =
    {
        .vmajor              = UCM_API_MAJOR_VER,
        .vminor              = UCM_API_MINOR_VER
    },
    .core.info.sys           = UCM_TYPE_PLUG_GUI,
    .core.info.vmajor        = UCM_VERSION_MAJOR,
    .core.info.vminor        = UCM_VERSION_MINOR,
    .core.info.vpatch        = UCM_VERSION_PATCH,
    .core.info.flags         = 0,
    .core.info.build         =
    {
        .commit              = UCM_BUILD_COMMIT,
        .datetime            = UCM_BUILD_TIME,
        .target              = UCM_BUILD_TARGET,
        .compiler            = UCM_BUILD_CC,
        .options             = UCM_BUILD_OPTS,
        .flags               = UCM_BUILD_FLAGS,
    },
    .core.info.pid           = L"uicurses",
    .core.info.name          = L"Ncurses TUI plugin",
    .core.info.developer     = L"SkyMaverick",
    .core.info.description   = L"Console interface TUI plugin based on ncurses library",
    .core.info.copyright     = L"Zlib",
    .core.info.email         = L"mail@mail.ru",
    .core.info.website       = L"http://null.org",

    .core.run                = _run_plugin,
    .core.stop               = _stop_plugin,
    .core.message            = _message,
};

const ucm_uiplugin_t*  hplug = &plugin;

LIBUCM_API ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
