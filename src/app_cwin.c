
#include "app.h"

static HMODULE core_handle;

static char pa_buf[UCM_PATH_MAX];
static char pla_buf[UCM_PATH_MAX];
static char ppa_buf[UCM_PATH_MAX];
static char psa_buf[UCM_PATH_MAX];

ucm_cargs_t args = { .path_abs       = pa_buf,
                     .path_lib_abs   = pla_buf,
                     .path_plug_abs  = ppa_buf,
                     .path_store_abs = psa_buf,

                     .options = 0 };

ucm_cstart_func core_start = NULL;
ucm_cstop_func core_stop   = NULL;
ucm_cinfo_func core_info   = NULL;

static ucm_plugin_info_t* info;

#include "loader.c"

void
event_load_hook(uint32_t eid, uintptr_t ev, uint32_t x1, uint32_t x2, void* ctx)
{
    UNUSED(eid);
    UNUSED(ev);
    UNUSED(x1);
    UNUSED(x2);
    UNUSED(ctx);

    fprintf(stdout, "[%s] %s\n", TUI_APP_NAME, _("Working hook LOAD_SUCCESS"));

    ucm_signal_t* sig = ucm_api->app.mainloop_sig_alloc(UCM_SIG_START_GUI);
    if (sig) {
        snprintf(U_SIGNAL_GUI(sig)->pid, UCM_PID_MAX, "%s", "uicurses");
        ucm_api->app.mainloop_sig_push(sig, 0, 0, NULL);
    }
    // TODO remove hook
}

static void
exit_func(int ret_status)
{
    if (info)
        free(info);

    if (core_stop)
        core_stop();

    FreeLibrary(core_handle);

    exit(ret_status);
}

BOOL
TermHandler(DWORD fwdHandlerType)
{
    fprintf(stderr, "[%s] %s\n", TUI_APP_NAME, _("Catch signal ..."));

    switch (fwdHandlerType) {
    case CTRL_C_EVENT:
    case CTRL_CLOSE_EVENT:
    case CTRL_SHUTDOWN_EVENT:
    default:
        exit_func(UCM_RET_SUCCESS);
    }
    return TRUE;
}

int
main(int argc, char* argv[])
{
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
            snprintf(tmp, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, PATH_DELIM, TUI_APP_NAME);
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

    //
    BOOL fRet = SetConsoleCtrlHandler((PHANDLER_ROUTINE)TermHandler, TRUE);
    if (!fRet)
        fprintf(stderr, "%s\n", "Couldn't set control handlers.");

    load_core_library(&args, event_load_hook);

    return ret_status;
}
