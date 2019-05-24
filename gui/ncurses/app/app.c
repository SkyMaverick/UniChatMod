#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>

#include "ucm.h"

#include "app.h"
#include "gettext.h"
#include "config.h"

#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)

    #include <signal.h>
    #include <execinfo.h>
#endif

#if defined (UCM_OS_POSIX)
    #include <getopt.h>
#else
    #include "getopt_win.h"
#endif

#if defined (UCM_OS_POSIX)
    #define LIBCORE_NAME "libucm.so"
    #define LIBCORE_PATH_SEPARATOR '/'
#else
    #define LIBCORE_NAME "ucm.dll"
    #define LIBCORE_PATH_SEPARATOR '\\'
#endif
#define LIBCORE_API_MAJVER  1
#define LIBCORE_API_MINVER  1

#define STACK_TRACE_BUFFER  4096

static char pa_buf  [UCM_PATH_MAX];
static char ppa_buf [UCM_PATH_MAX];
static char psa_buf [UCM_PATH_MAX];

static ucm_cargs_t args = {
   .path_abs        = pa_buf,
   .path_plug_abs   = ppa_buf,
   .path_store_abs  = psa_buf,

   .options         = 0
};

#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)
    static void*        core_handle;
#else
    static HMODULE      core_handle;
#endif

static const        ucm_plugin_info_t* info;

static int portable = 0;
static int portable_base = 0;
static int terminated = 0;

static inline void
_display_help (void)
{
    fprintf (stdout, "%s\n", "Usage UniChatMod tui mode: ucmc [options]");
    fprintf (stdout, "%s\n", "Options:");
    fprintf (stdout, "%s\n", "    -?              help");
    fprintf (stdout, "%s\n", "    -v              program version");
    fprintf (stdout, "%s\n", "    -r              read-only database mode");
    fprintf (stdout, "%s\n", "    -p <base path>  load or create external database file");
}

static inline void
_display_version (void)
{
    fprintf (stdout, "%s\n", UCM_VERSION);
}

static char*
app_realpath (const char*  path)
{
#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)
    return realpath(path, NULL);
#else
    char* tmp_path = NULL;
    HANDLE handle = CreateFileA(path,
                         0,
                         0,
                         NULL,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL | FILE_FLAG_BACKUP_SEMANTICS,
                         NULL);
    if (handle = INVALID_HANDLE_VALUE)
        return NULL;

    size_t path_len = GetFinalPathNameByHandleA(handle, NULL, 0, VOLUME_NAME_DOS);
    if (path_len) {
        tmp_path = malloc ((path_len + 1) * sizeof(TCHAR));
        if (tmp_path == NULL) {
            ZeroMemory(tmp_path, (path_len + 1) * sizeof(TCHAR));
            GetFinalPathNameByHandleA(handle, tmp_path, path_len, VOLUME_NAME_DOS);
        }
    }
    CloseHandle(handle);
    return tmp_path;
#endif
}

static void
_args_parse (int argc, char* argv[])
{
    int opt = 0;
    while ( ( opt = getopt(argc, argv, "rs:p:v?") ) != -1 )
    {
        switch (opt) {
            case '?':
                {
                    _display_help();
                    terminated = 1;
                    break;
                }
            case 'v':
                {
                    _display_version();
                    terminated = 1;
                    break;
                }
            case 'r':
                {
                    args.options |= UCM_FLAG_CORE_DBRO;
                    break;
                }
            case 's':
                {
                    // TODO server for proto list
                    break;
                }
            case 'p':
            /* Change custom storage file */
                {
                    char* rpath = app_realpath(optarg);
                    if (rpath) {
                        snprintf (args.path_store_abs, UCM_PATH_MAX, "%s", rpath);
                        free (rpath);
                    } else {
                        snprintf (args.path_store_abs, UCM_PATH_MAX, "%s", optarg);
                    }
                    portable_base = 0;

                    args.options |= UCM_FLAG_CORE_DBNEW;
                    break;
                }
        }
    }
}

#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)
static inline void
_stack_trace (int sig)
{
    fprintf (stderr, "[%s] %s\n", TUI_APP_NAME,_("Catch SEGV signal ..."));
    void* buf[STACK_TRACE_BUFFER];
    char** strs;

    int ptrs = backtrace(buf, STACK_TRACE_BUFFER);
    if (ptrs)
        fprintf (stderr, "%s: %d\n",_("Trace last addresses"), ptrs);

    strs = backtrace_symbols(buf, ptrs);
    if (strs != NULL) {
        for (int i=0; i < ptrs; i++)
            fprintf (stderr, "%s\n", strs[i]);
    } else fprintf (stderr, "%s\n",_("backtrace symbols error"));
    free(strs);
}

static void
_crash_handler (int sig)
{
#ifdef DEBUG
    _stack_trace (sig);
#endif
    // TODO maybe clean
    exit (UCM_RET_EXCEPTION);
}

#endif

int
main (int argc, char* argv[])
{
    portable = 1;
    int ret_status = UCM_RET_SUCCESS;
// *********************************************************
//      DEFINE STARTUP FILES AND CHECK ENV PATHS
// *********************************************************

#ifdef ENABLE_BUNDLE
        portable_base = 1;
#endif
    char* rpath = app_realpath(argv[0]);
    if (rpath) {
        snprintf (args.path_abs, UCM_PATH_MAX, "%s", rpath);
        free (rpath);
    } else {
        snprintf (args.path_abs, UCM_PATH_MAX, "%s", argv[0]);
    }

    char* e = strrchr(args.path_abs, '/');
    if (e) *e = '\0';

    /* check portable application objects */
    while (!portable || !portable_base) {
        char tmp [UCM_PATH_MAX];

        if (!portable) {
            // TODO
            snprintf (tmp, UCM_PATH_MAX, "%s%c%ls", args.path_abs, LIBCORE_PATH_SEPARATOR, UCM_PATH_MODULES);
                break;
            portable = 1;
        }
        if (!portable_base) {
            snprintf (tmp, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, LIBCORE_PATH_SEPARATOR, TUI_APP_NAME);
                break;
            portable_base = 1;
        }
        break;
    }

    if (portable) {
        snprintf (args.path_plug_abs, UCM_PATH_MAX, "%s%c%ls", args.path_abs, LIBCORE_PATH_SEPARATOR, UCM_PATH_MODULES);
    } else {
        //TODO
    }

    if (portable_base ) {
        snprintf (args.path_store_abs, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, LIBCORE_PATH_SEPARATOR, UCM_DB_DEFAULT_NAME);
    } else {
        // TODO
    }

    _args_parse (argc, argv);

    if (terminated)
        return ret_status;

#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)
    signal (SIGINT, _crash_handler);
    signal (SIGSEGV, _crash_handler);
#endif /* POSIX or WINEMU */

#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)

    core_handle = dlopen (LIBCORE_NAME, RTLD_LAZY);
    if (core_handle) {
        ucm_cstart_func core_start = dlsym (core_handle, UCM_START_FUNC);
        ucm_cstop_func  core_stop  = dlsym (core_handle, UCM_STOP_FUNC);
        ucm_cinfo_func  core_info  = dlsym (core_handle, UCM_INFO_FUNC);
#else
    core_handle = LoadLibraryA (LIBCORE_NAME);
    if (core_handle != INVALID_HANDLE_VALUE) {
        ucm_cstart_func core_start = (ucm_cstart_func)GetProcAddress(core_handle, UCM_START_FUNC);
        ucm_cstop_func  core_stop  = (ucm_cstop_func) GetProcAddress(core_handle, UCM_STOP_FUNC);
        ucm_cinfo_func  core_info  = (ucm_cinfo_func) GetProcAddress(core_handle, UCM_INFO_FUNC);
#endif
        if ( core_start && core_stop && core_info ) {
            core = core_start (&args);
            if (core) {
                info = core_info();
                if (   (info)
                    && (info->api.vmajor >= LIBCORE_API_MAJVER)
                    && (info->api.vminor >= LIBCORE_API_MINVER))
                {
                    // start curses
                    ucm_ev_t* ev = core->app.mainloop_ev_alloc (UCM_EVENT_START_GUI);
                    if (ev) {
                        snprintf ( U_EVENT_GUI(ev)->pid, UCM_PID_MAX, "%s", "uincurses");
                        core->app.mainloop_ev_push(ev, 0, 0, NULL);
                    }
//                    core->app.mainloop_msg_send (UCM_EVENT_START_GUI, (uintptr_t)L"uincurses",0,0);
                } else {
                    fprintf (stderr, "%s\n", "Core information load FAIL");
                    ret_status = UCM_RET_EMPTY;
                }
                core_stop();
            } else {
                fprintf (stderr, "%s\n", "Core API load FAIL");
                ret_status = UCM_RET_UNREALIZED;
            }
        }
#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)

    #ifndef ENABLE_VALGRIND
            dlclose(core_handle);
    #endif
#else
    FreeLibrary (core_handle);
#endif
    }
    else {
        fprintf (stderr, "%s: %s\n", "Don't load core library", LIBCORE_NAME);
        ret_status = UCM_RET_NOOBJECT;
    }
    return ret_status;
}
