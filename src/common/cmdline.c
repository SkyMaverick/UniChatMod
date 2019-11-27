#include "app.h"
#include "config.h"
#include "osal.h"
#include "ucm.h"

#if defined(UCM_OS_POSIX)
    #include <getopt.h>
#else
    #include "getopt_win.h"
#endif

static inline void
_display_help(void) {
    fprintf(stdout, "%s\n", "Usage UniChatMod tui mode: ucmc [options]");
    fprintf(stdout, "%s\n", "Options:");
    fprintf(stdout, "%s\n", "    -?              help");
    fprintf(stdout, "%s\n", "    -v              program version");
    fprintf(stdout, "%s\n", "    -r              read-only database mode");

    fprintf(stdout, "%s\n", "    -p <base path>  load or create external database file");
}

static inline void
_display_version(void) {
    fprintf(stdout, "%s\n", UCM_VERSION);
}

void
app_args_parse(int argc, char* argv[], ucm_cargs_t* args) {
    int opt = 0;
    while ((opt = getopt(argc, argv, "rs:p:vV?")) != -1) {
        switch (opt) {
        /* display help info */
        case '?':
            _display_help();
            set_flag(FLAG_APP_TERMINATED);
            break;
        /* display version application info */
        case 'v':
            _display_version();
            set_flag(FLAG_APP_TERMINATED);
            break;
        case 'V':
            set_flag(FLAG_APP_TERMINATED);
            break;
        case 'r':
            //            args->options |= UCM_FLAG_MODE_READONLY;
            break;
        case 's':
            // TODO server for proto list
            break;
        case 'p': {
            /* Change custom storage file */
            char* rpath = app_realpath(optarg);
            if (rpath) {
                snprintf(args->path_store_abs, UCM_PATH_MAX, "%s", rpath);
                free(rpath);
            } else {
                snprintf(args->path_store_abs, UCM_PATH_MAX, "%s", optarg);
            }
            unset_flag(FLAG_APP_PORTABLE_BASE);
            break;
        }
        }
    }
}
