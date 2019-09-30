
#include "app.h"

#include "argcmd.h"
#include "config.h"
#include "gettext.h"
#include "osal.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)

    #include <execinfo.h>
    #include <signal.h>
#endif

#if defined(UCM_OS_POSIX)
    #define LIBCORE_NAME "libucm.so"
#else
    #define LIBCORE_NAME "ucm.dll"
#endif
#define LIBCORE_API_MAJVER 1
#define LIBCORE_API_MINVER 1

#define STACK_TRACE_BUFFER 4096

#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)
static void* core_handle;
#else
static HMODULE core_handle;
#endif

const ucm_functions_t* core;

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

static uint32_t global_flags = 0;

const bool
get_flag(const app_flag_t flag)
{
    return (global_flags & flag);
}
void
set_flag(const app_flag_t flag)
{
    global_flags |= flag;
}
void
unset_flag(const app_flag_t flag)
{
    global_flags &= ~flag;
}

void
event_load_hook(uint32_t eid, uintptr_t ev, uint32_t x1, uint32_t x2, void* ctx)
{
    fprintf(stdout, "[%s] %s\n", TUI_APP_NAME, _("Working hook LOAD_SUCCESS"));

    ucm_signal_t* sig = core->app.mainloop_sig_alloc(UCM_SIG_START_GUI);
    if (sig) {
        snprintf(U_SIGNAL_GUI(sig)->pid, UCM_PID_MAX, "%s", "uicurses");
        core->app.mainloop_sig_push(sig, 0, 0, NULL);
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
#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)

    #ifndef ENABLE_VALGRIND
    dlclose(core_handle);
    #endif
#else
    FreeLibrary(core_handle);
#endif
    exit(ret_status);
}

#if defined(UCM_OS_WINDOWS)
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
#else
    #ifdef DEBUG
static void
_stack_trace(int sig)
{
    fprintf(stderr, "[%s] %s\n", TUI_APP_NAME, _("Catch SEGV signal ..."));
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
_crash_handler(int sig)
{
    #ifdef DEBUG
    _stack_trace(sig);
    #endif
    // TODO maybe clean
    exit(UCM_RET_EXCEPTION);
}

static void
_term_handler(int sig)
{
    fprintf(stderr, "[%s] %s\n", TUI_APP_NAME, _("Catch TERM signal ..."));
    exit_func(UCM_RET_SUCCESS);
}
#endif

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
#if defined(UCM_OS_WINDOWS)
    BOOL fRet = SetConsoleCtrlHandler((PHANDLER_ROUTINE)TermHandler, TRUE);
    if (!fRet)
        fprintf(stderr, "%s\n", "Couldn't set control handlers.");
#else
    signal(SIGINT, _crash_handler);
    signal(SIGSEGV, _crash_handler);
    signal(SIGTERM, _term_handler);
#endif /* POSIX or WINEMU */

#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)

    core_handle = dlopen(LIBCORE_NAME, RTLD_LAZY);
    if (core_handle) {
        core_start = dlsym(core_handle, UCM_START_FUNC);
        core_stop  = dlsym(core_handle, UCM_STOP_FUNC);
        core_info  = dlsym(core_handle, UCM_INFO_FUNC);
#else
    #ifdef DEBUG
    fprintf(stdout, "%s: %s\n", "Library search path", args.path_lib_abs);
    #endif

    SetDllDirectoryA(args.path_lib_abs);
    core_handle = LoadLibraryA(LIBCORE_NAME);
    if (core_handle != INVALID_HANDLE_VALUE) {
        core_start = (ucm_cstart_func)GetProcAddress(core_handle, UCM_START_FUNC);
        core_stop  = (ucm_cstop_func)GetProcAddress(core_handle, UCM_STOP_FUNC);
        core_info  = (ucm_cinfo_func)GetProcAddress(core_handle, UCM_INFO_FUNC);
#endif
        if (core_start && core_stop && core_info) {
            info = malloc(sizeof(ucm_plugin_info_t));
            if (info) {
                core_info(UCM_INFO_CORE, (void*)info, sizeof(ucm_plugin_info_t), &args);
                if ((info->api.vmajor >= LIBCORE_API_MAJVER) &&
                    (info->api.vminor >= LIBCORE_API_MINVER)) {
                    app_args_parse(argc, argv, &args);

                    if (get_flag(FLAG_APP_TERMINATED))
                        return ret_status;

                    core = core_start(&args);
                    if (core) {
                        core->app.mainloop_hook_attach(event_load_hook, NULL, UCM_SIG_LOAD_SUCCESS);
                        core->app.wait_exit();

                        core->app.mainloop_hook_detach(event_load_hook);
                        exit_func(UCM_RET_SUCCESS);
                    } else {
                        fprintf(stderr, "%s\n", "Core API load FAIL");
                        exit_func(UCM_RET_NOTIMPLEMENT);
                    }
                }
            } else {
                // TODO
                exit_func(UCM_RET_EXCEPTION);
            }
        } else {
            fprintf(stderr, "%s\n", "Core information load FAIL");
            exit_func(UCM_RET_EMPTY);
        }
    } else {
        ret_status = UCM_RET_SYSTEM_DLERROR;
    }

    return ret_status;
}
