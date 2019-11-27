#include "tui.h"

static npc_window_t* win_main;

static uintptr_t
_curses_init(void) {
    win_main = malloc(NPC_MEMNEED_WINDOW);
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
        return 0;
    }
    return (uintptr_t)win_main;
}

static void
cb_read_alloc(uv_handle_t* handle, size_t suggested, uv_buf_t* buf) {
    *buf = uv_buf_init((char*)ucm_api->sys.malloc(suggested), suggested);
}

static void
cb_read_stdin_nonblock(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
    if (buf->base)
        ucm_api->sys.free(buf->base);
}

void*
curses_dispatch(void* ctx) {

    uv_loop_t* loop = UCM_LOOP_SYSTEM(ucm_api);
    uv_tty_t tty;

    ucm_api->uv.tty_init(loop, &tty, STDIN_FILENO, 1);
    ucm_api->uv.tty_set_mode(&tty, UV_TTY_MODE_NORMAL);

    if ((ucm_api->uv.guess_handle(1) == UV_TTY) && (ucm_api->uv.is_readable((uv_stream_t*)&tty))) {
        ucm_api->uv.read_start((uv_stream_t*)&tty, cb_read_alloc, cb_read_stdin_nonblock);
    }
    ucm_api->uv.run(loop, UV_RUN_DEFAULT);

    //    _curses_uvwrap();
    //
    //    int ch = 0;
    //    // TODO Temporary F1 - it's exit
    //    while ((ch = getch()) != KEY_F(1) || get_flag(FLAG_APP_TERMINATED)) {
    //        timeout(3000);
    //        addch(ch);
    //    }
    //    if (!get_flag(FLAG_APP_TERMINATED))
    //        ucm_api->app.mainloop_msg_send(UCM_SIG_TERM, 0, 0, 0);
    //
    return NULL;
}

static void
_curses_finish(void) {
    endwin();

    if (win_main)
        free_and_null(win_main);
}

uintptr_t
curses_start(void) {
    return _curses_init();
}

void
curses_finish(void) {
    _curses_finish();
}
