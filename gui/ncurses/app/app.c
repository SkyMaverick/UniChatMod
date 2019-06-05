#if defined (UCM_OS_WINDOWS) && \
    defined (DEBUG)
    #define _DEBUG
    #define _CRTDBG_MAP_ALLOC

    #include <stdlib.h>
    #include <crtdbg.h>
#else
    #include <stdlib.h>
#endif

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>

#include "ucm.h"
#include "config.h"

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
#else
    #define LIBCORE_NAME "ucm.dll"
#endif
#define LIBCORE_API_MAJVER  1
#define LIBCORE_API_MINVER  1

#define STACK_TRACE_BUFFER  4096


static char pa_buf  [UCM_PATH_MAX];
static char pla_buf [UCM_PATH_MAX];
static char ppa_buf [UCM_PATH_MAX];
static char psa_buf [UCM_PATH_MAX];

static ucm_cargs_t args = {
   .path_abs        = pa_buf,
   .path_lib_abs    = pla_buf,
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

static ucm_cstart_func core_start = NULL;
static ucm_cstop_func  core_stop  = NULL;
static ucm_cinfo_func  core_info  = NULL;

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

static inline int
app_realloc (void** mem,
             size_t size)
{
    void* old_mem = *mem;
    *mem = realloc (*mem, size);
    if ( *mem != old_mem ) {
        if ( *mem == NULL ) {
            *mem = old_mem;
            return 1;
        }
    }
    return 0;
}

static char*
app_mypath (char* path)
{
#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)
    return app_realpath(path);
#else
    size_t size = UCM_PATH_MAX;

    char* tmp_path = malloc((size + 1) * sizeof(TCHAR));
    if (tmp_path) {
        do {
            size_t r = GetModuleFileNameA(NULL, (LPSTR)tmp_path, size);
            if ( (r < size) && (r != 0)) {
                size = r;
                if (app_realloc (&tmp_path, size + 1) != 0) {
                    free (tmp_path);
                    goto bailout;
                }
                tmp_path [size] = '\0';
                break;
            }

            size *= 2;
            if ( app_realloc ( &tmp_path, size + 1 ) != 0) {
                free (tmp_path);
                goto bailout;
            }
        } while(1);
        return tmp_path;
    }
    bailout: return 0;
#endif
}


static void
exit_func (int ret_status)
{
    if (core_stop)
        core_stop ();
#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)

    #ifndef ENABLE_VALGRIND
        dlclose(core_handle);
    #endif
#else
    FreeLibrary (core_handle);
#endif
    exit (ret_status);
}

#if defined (UCM_OS_WINDOWS)
    BOOL
    TermHandler (DWORD fwdHandlerType)
    {
        fprintf (stderr, "[%s] %s\n", TUI_APP_NAME,_("Catch signal ..."));
        switch (fwdHandlerType) {
            default:
                {
                    //TODO
                }
        }
        return TRUE;
    }
#else
    static void
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

    static void
    _term_handler (int sig)
    {
        fprintf (stderr, "[%s] %s\n", TUI_APP_NAME,_("Catch TERM signal ..."));
        exit_func(UCM_RET_SUCCESS);
    }
#endif

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


int
main (int argc, char* argv[])
{
#if defined (UCM_OS_WINDOWS) && \
    defined (DEBUG)
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif
    portable = 1;
    int ret_status = UCM_RET_SUCCESS;
// *********************************************************
//      DEFINE STARTUP FILES AND CHECK ENV PATHS
// *********************************************************

#ifdef ENABLE_BUNDLE
        portable_base = 1;
#endif
    char* rpath = app_mypath(argv[0]);
    if (rpath) {
        snprintf (args.path_abs, UCM_PATH_MAX, "%s", rpath);
        free (rpath);
    } else {
        snprintf (args.path_abs, UCM_PATH_MAX, "%s", argv[0]);
    }

#ifdef DEBUG
    fprintf (stdout, "%s: %s\n", "Start application", args.path_abs);
#endif

    char* e = strrchr(args.path_abs, PATH_DELIM);
    if (e) *e = '\0';

#ifdef DEBUG
    fprintf (stdout, "%s: %s\n", "Application path", args.path_abs);
#endif


    /* check portable application objects */
    while (!portable || !portable_base) {
        char tmp [UCM_PATH_MAX];

        if (!portable) {
            // TODO
            snprintf (tmp, UCM_PATH_MAX, "%s%c%ls", args.path_abs, PATH_DELIM, UCM_PATH_MODULES);
                break;
            portable = 1;
        }
        if (!portable_base) {
            snprintf (tmp, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, PATH_DELIM, TUI_APP_NAME);
                break;
            portable_base = 1;
        }
        break;
    }

    if (portable) {
        snprintf (args.path_lib_abs,  UCM_PATH_MAX, "%s%c%ls", args.path_abs, PATH_DELIM, UCM_PATH_DEPENDS);
        snprintf (args.path_plug_abs, UCM_PATH_MAX, "%s%c%ls", args.path_abs, PATH_DELIM, UCM_PATH_MODULES);
    } else {
        //TODO
    }

    if (portable_base ) {
        snprintf (args.path_store_abs, UCM_PATH_MAX, "%s%c%s.mdbx", args.path_abs, PATH_DELIM, UCM_DB_DEFAULT_NAME);
    } else {
        // TODO
    }

    _args_parse (argc, argv);

    if (terminated)
        return ret_status;

#if defined (UCM_OS_WINDOWS)
    BOOL fRet = SetConsoleCtrlHandler (
                (PHANDLER_ROUTINE) TermHandler,
                TRUE
            );
    if ( !fRet )
        fprintf(stderr, "%s\n", "Couldn't set control handlers.");
#else
    signal (SIGINT, _crash_handler);
    signal (SIGSEGV, _crash_handler);
    signal (SIGTERM, _term_handler);
#endif /* POSIX or WINEMU */

#if defined (UCM_OS_POSIX) || \
    defined (UCM_OS_WINEMULATOR)

    core_handle = dlopen (LIBCORE_NAME, RTLD_LAZY);
    if (core_handle) {
        core_start = dlsym (core_handle, UCM_START_FUNC);
        core_stop  = dlsym (core_handle, UCM_STOP_FUNC);
        core_info  = dlsym (core_handle, UCM_INFO_FUNC);
#else
#ifdef DEBUG
    fprintf (stdout, "%s: %s\n", "Library search path", args.path_lib_abs);
#endif

    SetDllDirectoryA(args.path_lib_abs);
    core_handle = LoadLibraryA (LIBCORE_NAME);
    if (core_handle != INVALID_HANDLE_VALUE) {
        core_start = (ucm_cstart_func)GetProcAddress(core_handle, UCM_START_FUNC);
        core_stop  = (ucm_cstop_func) GetProcAddress(core_handle, UCM_STOP_FUNC);
        core_info  = (ucm_cinfo_func) GetProcAddress(core_handle, UCM_INFO_FUNC);
#endif
        if ( core_start && core_stop && core_info ) {
            info = core_info();
            if (info
                && (info->api.vmajor >= LIBCORE_API_MAJVER)
                && (info->api.vminor >= LIBCORE_API_MINVER))
            {
                core = core_start (&args);
                if (core) {
                    ucm_ev_t* ev = core->app.mainloop_ev_alloc (UCM_EVENT_START_GUI);
                    if (ev) {
                      snprintf ( U_EVENT_GUI(ev)->pid, UCM_PID_MAX, "%s", "uincurses");
                      core->app.mainloop_ev_push(ev, 0, 0, NULL);
                    }
                    core->app.wait_exit();
                    exit_func (UCM_RET_SUCCESS);
                } else {
                    fprintf (stderr, "%s\n", "Core API load FAIL");
                    exit_func (UCM_RET_UNREALIZED);
                }
            }
        } else {
            fprintf (stderr, "%s\n", "Core information load FAIL");
            exit_func (UCM_RET_EMPTY);
        }
    } else {
        fprintf (stderr, "%s: %s\n", "Don't load core library", LIBCORE_NAME);
        ret_status = UCM_RET_NOOBJECT;
    }


    return ret_status;
}
