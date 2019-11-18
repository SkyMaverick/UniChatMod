#pragma once

#define APP_NAME "ucmc"

#define ONLY_WITH_CLIENT_API 1

#include "config.h"
#include "ucm.h"

#include "osal.h"
#include "cmdline.h"
#include "flags.h"

#ifdef ENABLE_CURSES_UI
    #include "curses/tui.h"
#endif

#if defined(UCM_OS_POSIX)
    #define LIBCORE_NAME "libucm.so"
#else
    #define LIBCORE_NAME "ucm.dll"
#endif
#define LIBCORE_API_MAJVER 1
#define LIBCORE_API_MINVER 1

#define STACK_TRACE_BUFFER 4096

extern ucm_functions_t* ucm_api;

#define free_and_null(X)                                                                           \
    do {                                                                                           \
        free((X));                                                                                 \
        (X) = NULL;                                                                                \
    } while (0)
