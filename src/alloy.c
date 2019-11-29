
#include "app.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)
    #include <execinfo.h>
    #include <signal.h>
#endif

ucm_functions_t* ucm_api;

#if defined(UCM_OS_POSIX)
    #include "posix/app.c"
#else
    #include "win/app.c"
#endif
