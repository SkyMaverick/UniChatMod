#pragma once

#include "umacro.h" // THIS MUST BE FIRST !!!

#if defined(UCM_OS_WINDOWS)
    #include "Windows.h"
#endif
#if defined(UCM_OS_POSIX)
    #include <dlfcn.h>
    #include <errno.h>
    #include <sys/stat.h>
    #include <sys/types.h>
    #include <unistd.h>
#endif

#if defined(UCM_OS_WINDOWS)
    #define DYNLIB_SUFFIX ".dll"
#elif defined(UCM_OS_DARWIN)
    #define DYNLIB_SUFFIX ".dynlib"
#else
    #define DYNLIB_SUFFIX ".so"
#endif

#if defined(UCM_OS_WINDOWS)
    #define PATH_DELIM '\\'
#else
    #define PATH_DELIM '/'
#endif

#define ENABLE_WARN_DEPRECATED 1

#define UCM_API_MAJOR_VER 1
#define UCM_API_MINOR_VER 1

// === CONSTANTS ========================

#if defined(PATH_MAX)
    #define UCM_PATH_MAX PATH_MAX
#else
    #define UCM_PATH_MAX 4096
#endif

#if defined(NAME_MAX)
    #define UCM_NAME_MAX NAME_MAX
#elif defined(_UCM_CONFIG_H_)
    #define UCM_NAME_MAX DEF_NAME_MAX
#else
    #define UCM_NAME_MAX 256
#endif

#define UCM_PID_MAX 50
#define UNUSED(x) (void)(x);

#define UCM_CONTACT_ID_MAX 256
#define UCM_CONTACT_NAME_MAX 1024

#define UCM_DB_DEFAULT_NAME "ucmdb"
