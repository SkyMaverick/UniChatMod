#include "tui.h"

#include "config.h"
#include "ucm.h"

#include <curses.h>
#include <string.h>

#ifdef UCM_OS_POSIX
    #include <sys/time.h>
    #include <sys/types.h>
    #include <unistd.h>
#endif

const ucm_functions_t* app;

static uintptr_t hwnd = 0;
static int term       = 1;

static char
_curses_mtgetch()
{

    char input = '\0';
#ifdef UCM_OS_POSIX
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);

    struct timeval tv;
    tv.tv_sec  = 5;
    tv.tv_usec = 0;

    int status = select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv);
    if (status == -1) {
        if (errno != EINTR) {
            // TODO return error
            return '\0';
        }
    } else if (FD_ISSET(STDIN_FILENO, &fds)) {
        if ((input = getch()) == ERR) {
            // TODO return error
            return '\0';
        }
    }
#endif
    return input;
}

static void
_curses_start(void)
{
    initscr();
    keypad(stdscr, TRUE);
    (void)nonl();
    (void)cbreak();
    (void)noecho();

    if (has_colors()) {
        start_color();

        init_pair(COLOR_BLACK, COLOR_BLACK, COLOR_BLACK);
        init_pair(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
        init_pair(COLOR_RED, COLOR_RED, COLOR_BLACK);
        init_pair(COLOR_CYAN, COLOR_CYAN, COLOR_BLACK);
        init_pair(COLOR_WHITE, COLOR_WHITE, COLOR_BLACK);
        init_pair(COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK);
        init_pair(COLOR_BLUE, COLOR_BLUE, COLOR_BLACK);
        init_pair(COLOR_YELLOW, COLOR_YELLOW, COLOR_BLACK);
    }
    refresh();
}

static void
_curses_cleanup(void)
{
    endwin();
}

static void*
_curses_loop(void* ctx)
{
    UNUSED(ctx);
    fprintf(stdout, "%s\n", "Start curses APP");
    //    _curses_start();

    while (term) {
        char c = _curses_mtgetch();
        if (c == 'q') {
            app->app.terminate(0, 0, 0);
            break;
        }
    }

    //    _curses_cleanup();
    app->sys.thread_exit();
    return NULL;
}

// *********************************************************
//      EXTERNAL FUNCTIONS
// *********************************************************

UCM_RET
start_curses_app(void* ctx)
{
    hwnd = app->sys.thread_create(_curses_loop, ctx);
    //    app->sys.thread_join(hwnd);
    return UCM_RET_SUCCESS;
}

void
finish_curses_app(int sig)
{
    UNUSED(sig);
    // TODO
}

// *********************************************************
//      PLUGIN FUNCTIONS
// *********************************************************

static UCM_RET
_run_plugin(void)
{
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_plugin(void)
{
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2)
{
    switch (id) {
    case UCM_SIG_START_GUI: {
        if (strcmp(plug->core.info.pid, (char*)ctx)) {
            start_curses_app((void*)ctx);
        }
        break;
    }

    case UCM_SIG_TERM: {
        term = 0;
        break;
    }
    };
    UNUSED(x1);
    UNUSED(x2);
}

static ucm_plugui_t plugin = {
    .core.oid         = UCM_TYPE_OBJECT_PLUGIN,
    .core.info.api    = {.vmajor = UCM_API_MAJOR_VER, .vminor = UCM_API_MINOR_VER},
    .core.info.sys    = UCM_TYPE_PLUG_GUI,
    .core.info.vmajor = UCM_VERSION_MAJOR,
    .core.info.vminor = UCM_VERSION_MINOR,
    .core.info.vpatch = UCM_VERSION_PATCH,
    .core.info.flags  = 0,
    .core.info.build =
        {
            .commit   = UCM_BUILD_COMMIT,
            .datetime = UCM_BUILD_TIME,
            .target   = UCM_BUILD_TARGET,
            .compiler = UCM_BUILD_CC,
            .options  = UCM_BUILD_OPTS,
            .flags    = UCM_BUILD_FLAGS,
        },
    .core.info.pid         = "c609b9d1-2965-4e1e-8639-91d5556115d9",
    .core.info.name        = L"std_uicurses",
    .core.info.developer   = L"SkyMaverick",
    .core.info.description = L"Console interface TUI plugin based on ncurses/pdcurses library",
    .core.info.copyright   = L"Zlib",
    .core.info.email       = L"mail@mail.ru",
    .core.info.website     = L"http://null.org",

    .core.run     = _run_plugin,
    .core.stop    = _stop_plugin,
    .core.message = _message,
};

const ucm_plugui_t* plug = &plugin;

LIBUCM_API ucm_plugin_t*
_init_plugin(const ucm_functions_t* api)
{
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
