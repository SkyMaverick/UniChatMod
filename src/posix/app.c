#include "app.h"

static void* core_handle;

static char pa_buf[UCM_PATH_MAX];
static char pla_buf[UCM_PATH_MAX];
static char ppa_buf[UCM_PATH_MAX];
static char psa_buf[UCM_PATH_MAX];

ucm_cargs_t args = {.path_abs = pa_buf,
                    .path_lib_abs = pla_buf,
                    .path_plug_abs = ppa_buf,
                    .path_store_abs = psa_buf};

ucm_func_load core_load = NULL;
ucm_func_exec core_exec = NULL;
ucm_func_unload core_unload = NULL;
ucm_func_info core_info = NULL;

static const ucm_plugin_info_t* info;

#include "loader.c"

void
event_load_hook(uint32_t eid, uintptr_t ev, uint32_t x1, uint32_t x2, void* ctx) {
    UNUSED(eid);
    UNUSED(ev);
    UNUSED(x1);
    UNUSED(x2);
    UNUSED(ctx);

    fprintf(stdout, "[%s] %s\n", APP_NAME, _("Working hook LOAD_SUCCESS"));

//    curses_dispatch(NULL);

    ucm_signal_t* sig = ucm_api->app.mainloop_sig_alloc(UCM_SIG_START_GUI);
    if (sig) {
        snprintf(U_SIGNAL_GUI(sig)->pid, UCM_PID_MAX, "%s", "uicurses");
        ucm_api->app.mainloop_sig_push(sig, 0, 0, NULL);
    }
    // TODO remove hook
}

static void
exit_func(int ret_status) {

    if (core_unload && ucm_api)
        core_unload(&ucm_api);

    dlclose(core_handle);
    exit(ret_status);
}

#ifdef DEBUG
static void
_stack_trace(int sig) {
    UNUSED(sig);

    fprintf(stderr, "[%s] %s\n", APP_NAME, _("Catch SEGV signal ..."));
    void* buf[STACK_TRACE_BUFFER];
    char** strs;

    int ptrs = backtrace(buf, STACK_TRACE_BUFFER);
    if (ptrs)
        fprintf(stderr, "%s: %d\n", _("Trace last addresses"), ptrs);

    strs = backtrace_symbols(buf, ptrs);
    if (strs != NULL) {
        for (int i = 0; i < ptrs; i++)
            fprintf(stderr, "%s\n", strs[i]);
    } else
        fprintf(stderr, "%s\n", _("backtrace symbols error"));
    free(strs);
}
#endif

static void
_crash_handler(int sig) {
#ifdef DEBUG
    _stack_trace(sig);
#endif
    // TODO maybe clean
    exit(UCM_RET_EXCEPTION);
}

static void
_term_handler(int sig) {
    UNUSED(sig);

    fprintf(stderr, "[%s] %s\n", APP_NAME, _("Catch TERM signal ..."));
    exit_func(UCM_RET_SUCCESS);
}

int
main(int argc, char* argv[]) {
    set_flag(FLAG_APP_PORTABLE);
    int ret_status = UCM_RET_SUCCESS;
    // *********************************************************
    //      DEFINE STARTUP FILES AND CHECK ENV PATHS
    // *********************************************************

#ifdef ENABLE_BUNDLE
    set_flag(FLAG_APP_PORTABLE_BASE);
#endif
    char* rpath = app_mypath(argv[0]);
    if (rpath) {
        snprintf(args.path_abs, UCM_PATH_MAX, "%s", rpath);
        free(rpath);
    } else {
        snprintf(args.path_abs, UCM_PATH_MAX, "%s", argv[0]);
    }

    char* e = strrchr(args.path_abs, PATH_DELIM);
    if (e)
        *e = '\0';

    /* check portable application objects */
    while (!get_flag(FLAG_APP_PORTABLE) || !get_flag(FLAG_APP_PORTABLE_BASE)) {
        char tmp[UCM_PATH_MAX];

        if (!get_flag(FLAG_APP_PORTABLE)) {
            // TODO
            snprintf(tmp, UCM_PATH_MAX, "%s%c%ls", args.path_abs, PATH_DELIM, UCM_PATH_MODULES);
            break;
            set_flag(FLAG_APP_PORTABLE);
        }
        if (!get_flag(FLAG_APP_PORTABLE_BASE)) {
            snprintf(tmp, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, PATH_DELIM, APP_NAME);
            break;
            set_flag(FLAG_APP_PORTABLE_BASE);
        }
        break;
    }

    if (get_flag(FLAG_APP_PORTABLE)) {
        snprintf(args.path_lib_abs, UCM_PATH_MAX, "%s%c%ls", args.path_abs, PATH_DELIM,
                 UCM_PATH_DEPENDS);
        snprintf(args.path_plug_abs, UCM_PATH_MAX, "%s%c%ls", args.path_abs, PATH_DELIM,
                 UCM_PATH_MODULES);
    } else {
        // TODO
    }

    if (get_flag(FLAG_APP_PORTABLE_BASE)) {
        snprintf(args.path_store_abs, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, PATH_DELIM,
                 UCM_DB_DEFAULT_NAME);
    } else {
        // TODO
    }

    signal(SIGINT, _crash_handler);
    signal(SIGSEGV, _crash_handler);
    signal(SIGTERM, _term_handler);

    app_args_parse(argc, argv, &args);

    if (get_flag(FLAG_APP_TERMINATED))
        return ret_status;
    
    ret_status = load_core_library(&args, event_load_hook);
    if (ret_status != UCM_RET_SYSTEM_DLERROR)
        exit_func(ret_status);

    return ret_status;
}
