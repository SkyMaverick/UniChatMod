#include <dlfcn.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <getopt.h>
#include <signal.h>
#include <execinfo.h>

#include "ucm.h"
#include "app.h"
#include "config.h"
#include "gettext.h"

#define LIBCORE_NAME "libucm.so"
#define LIBCORE_API_MAJVER  0
#define LIBCORE_API_MINVER  0

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

static void*        core_handle;
static const        ucm_plugin_info_t* info;

static int portable = 0;
static int portable_base = 0;
static int terminated = 0;

static inline void
_display_help (void)
{
    fprintf (stdout, _("Usage UniChatMod tui mode: ucmc [options]\n"));
    fprintf (stdout, _("Options:\n"));
    fprintf (stdout, _("    -?              help\n"));
    fprintf (stdout, _("    -v              program version\n"));
    fprintf (stdout, _("    -r              read-only database mode\n"));
    fprintf (stdout, _("    -p <base path>  load or create external database file\n"));
}

static inline void
_display_version (void)
{
    fprintf (stdout, "%s\n", UCM_VERSION);
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
                    struct stat st;
                    char tmp [UCM_PATH_MAX];

                    if ( !realpath (optarg, tmp) ) {
                        snprintf (tmp, UCM_PATH_MAX, "%s", optarg);
                    }
                    if ( stat(tmp, &st) && !S_ISREG (st.st_mode) )
                        break;
                    portable_base = 0;
                    strncpy (args.path_store_abs, tmp, UCM_PATH_MAX);

                    args.options |= UCM_FLAG_CORE_DBNEW;
                    break;
                }
        }
    }
}

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

int
main (int argc, char* argv[])
{
    int ret_status = UCM_RET_SUCCESS;
// *********************************************************
//      DEFINE STARTUP FILES AND CHECK ENV PATHS
// *********************************************************

#ifdef ENABLE_BUNDLE
    portable = 1;
    #ifdef BASE_INBUNDLE
        portable_base = 1;
    #endif
#endif

    if ( !realpath (argv[0], args.path_abs) ) {
        snprintf (args.path_abs, UCM_PATH_MAX, "%s", argv[0]);
    }

    char* e = strrchr(args.path_abs, '/');
    if (e) *e = '\0';

    /* check portable application objects */
    while (!portable || !portable_base) {
        struct stat st;
        char tmp [UCM_PATH_MAX];

        if (!portable) {
            // TODO
            snprintf (tmp, UCM_PATH_MAX, "%s/%ls", args.path_abs, UCM_PATH_MODULES);
            if (stat(tmp, &st) || !S_ISDIR(st.st_mode))
                break;
            portable = 1;
        }
        if (!portable_base) {
            snprintf (tmp, UCM_PATH_MAX, "%s/%s.mdbx", args.path_abs, TUI_APP_NAME);
            if (stat(tmp, &st) || !S_ISREG(st.st_mode))
                break;
            portable_base = 1;
        }
        break;
    }

    if (portable) {
        snprintf (args.path_plug_abs, UCM_PATH_MAX, "%s/%ls", args.path_abs, UCM_PATH_MODULES);
    } else {
        //TODO
    }

    if (portable_base ) {
        snprintf (args.path_store_abs, UCM_PATH_MAX, "%s/%s.mdbx", args.path_abs, TUI_APP_NAME);
    } else {
        // TODO
    }

    _args_parse (argc, argv);

    if (terminated)
        return ret_status;
// *********************************************************
//      SIGNAL HANDLERS
// *********************************************************

    signal (SIGINT, _crash_handler);
    signal (SIGSEGV, _crash_handler);

// *********************************************************
//      LOAD CORE LIBRARY
// *********************************************************

    core_handle = dlopen (LIBCORE_NAME, RTLD_LAZY);
    if (core_handle) {
        ucm_cstart_func core_start = dlsym (core_handle, UCM_START_FUNC);
        ucm_cstop_func  core_stop  = dlsym (core_handle, UCM_STOP_FUNC);
        ucm_cinfo_func  core_info =  dlsym (core_handle, UCM_INFO_FUNC);

        if ( core_start && core_stop && core_info ) {
            core = core_start (&args);
            if (core) {
                info = core_info();
        #pragma GCC diagnostic push
        #pragma GCC diagnostic ignored "-Wtype-limits"
                if (   (info)
                    && (info->api.vmajor >= LIBCORE_API_MAJVER)
                    && (info->api.vminor >= LIBCORE_API_MINVER))
                {
                    // start curses
                    core->mainloop_msg_send (UCM_EVENT_START_GUI, (uintptr_t)L"uincurses",0,0);
                } else {
                    fprintf (stderr, "%s\n", "Core information load FAIL");
                    ret_status = UCM_RET_EMPTY;
                }
        #pragma GCC diagnostic pop
                core_stop();
            } else {
                fprintf (stderr, "%s\n", "Core API load FAIL");
                ret_status = UCM_RET_UNREALIZED;
            }
        }
        dlclose(core_handle);
    }
    else {
        fprintf (stderr, "%s: %s\n", "Don't load core library", LIBCORE_NAME);
        ret_status = UCM_RET_NOOBJECT;
    }
    return ret_status;
}
