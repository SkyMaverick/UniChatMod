
#include "app.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)

    #include <execinfo.h>
    #include <signal.h>
#endif

const ucm_functions_t* ucm_api;

#if defined(UCM_OS_POSIX)
    #include "app_linux.c"
#else
    #if defined(WIN_CONSOLE_APP)
        #include "app_cwin.c"
    #else
        #include "app_win.c"
    #endif
#endif