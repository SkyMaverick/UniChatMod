#include <dlfcn.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <getopt.h>

#include "ucm.h"
#include "alloc.h"
#include "cli_config.h"

#define LIBCORE_NAME "libucm.so"
#define LIBCORE_API_MAJVER  0
#define LIBCORE_API_MINVER  0

char start_path [PATH_MAX];
char plugs_path [PATH_MAX];
char store_path [PATH_MAX];

static void* core_handle;
static const ucm_plugin_info_t* core_info;
const ucm_functions_t* core;

int portable = 0;
int portable_base = 0;

static void
_args_parse (int argc, char* argv[])
{
    int opt = 0;
    while ( ( opt = getopt(argc, argv, "s:p:") ) != -1 )
    {
        switch (opt) {
            case 'c':
                {
                    // TODO create new database
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
                    char tmp [PATH_MAX];
                    
                    if ( !realpath (optarg, tmp) ) {
                        snprintf (tmp, PATH_MAX, "%s", optarg);
                    }
                    if ( stat(tmp, &st) && !S_ISREG (st.st_mode) )
                        break;
                    portable_base = 0;
                    strncpy (store_path, tmp, PATH_MAX);

                    break;
                }
        }
    }
}

int
main (int argc, char* argv[])
{
// *********************************************************
//      DEFINE STARTUP FILES AND CHECK ENV PATHS
// *********************************************************

#ifdef ENABLE_BUNDLE
    portable = 1;
    #ifdef BASE_INBUNDLE
        portable_base = 1;
    #endif
#endif

    if ( !realpath (argv[0], start_path) ) {
        snprintf (start_path, PATH_MAX, "%s", argv[0]);
    }

    char* e = strrchr(start_path, '/');
    if (e) *e = 0;

    /* check portable application objects */
    while (!portable || !portable_base) {
        struct stat st;
        char tmp [PATH_MAX];

        if (!portable) {
            // TODO
            snprintf (tmp, PATH_MAX, "%s/%s", start_path, CLI_PATH_MODS);
            if (stat(tmp, &st) || !S_ISDIR(st.st_mode))
                break;
            strncpy (plugs_path, tmp, PATH_MAX);
            portable = 1;
        }
        if (!portable_base) {
            snprintf (tmp, PATH_MAX, "%s/%s.mdbx", start_path, CLI_APP_NAME);
            if (stat(tmp, &st) || !S_ISREG(st.st_mode))
                break;
            strncpy (store_path, tmp, PATH_MAX);
            portable_base = 1;
        }
        break;
    }
    _args_parse (argc, argv);
// *********************************************************
//      LOAD CORE LIBRARY
// *********************************************************

    core_handle = dlopen (LIBCORE_NAME, RTLD_LAZY);
    if (!core_handle) {
        fprintf (stderr, "%s: %s\n", "Don't load core library", LIBCORE_NAME);
        return UCM_RET_NOOBJECT;
    }

    core = ucm_core_start (start_path, plugs_path, store_path);
    if (core) {
        core_info = ucm_core_info();
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wtype-limits"
        if (   (core_info)
            && (core_info->api.vmajor >= LIBCORE_API_MAJVER)
            && (core_info->api.vminor >= LIBCORE_API_MINVER))
        {
            //TODO
        } else {
            fprintf (stderr, "%s\n", "Core information load FAIL");
        }
#pragma GCC diagnostic pop
        ucm_core_stop();
    } else {
        fprintf (stderr, "%s\n", "Core API load FAIL");
    }

    dlclose(core_handle);
    return UCM_RET_SUCCESS;
}
