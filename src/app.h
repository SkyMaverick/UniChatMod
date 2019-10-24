#pragma once

#define APP_NAME "ucmc"
#include "config.h"
#include "ucm.h"

#include "gettext.h"
#include "osal.h"
#include "cmdline.h"
#include "flags.h"

#include <stdbool.h>

#if defined(UCM_OS_POSIX)
    #define LIBCORE_NAME "libucm.so"
#else
    #define LIBCORE_NAME "ucm.dll"
#endif
#define LIBCORE_API_MAJVER 1
#define LIBCORE_API_MINVER 1

#define STACK_TRACE_BUFFER 4096

extern const ucm_functions_t* ucm_api;
