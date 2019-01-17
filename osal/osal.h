#pragma once

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

/* Platform specific includes */
#if defined(_WIN32) || defined(_WIN64)
    #pragma comment (lib, "Shlwapi.lib")
    
    #include <Windows.h>
    #include <WinNT.h>
    #include <Winternl.h>
    #include "Shlwapi.h"
#else
    #include <pthread.h>
    #include <signal.h>
    #include <sys/file.h>
    #include <sys/stat.h>
    #include <sys/select.h>
    #include <sys/types.h>
    #include <unistd.h>
    #include <fcntl.h>
    #include <dlfcn.h>
    #include <errno.h>
    #include <dirent.h>
    #include <malloc.h>
#endif

enum {
    OSAL_RETURN_SUCCESS,
    OSAL_RETURN_ERROR,
    OSAL_RETURN_ABORT,
    OSAL_RETURN_ETIMEOUT,
    OSAL_RETURN_EABANDONED,
    OSAL_RETURN_EIO,
    OSAL_RETURN_EACCESS,
    OSAL_RETURN_ENOMEM
};
/* **************************************************
    Dynamic library loader functions
 ************************************************** */

#if defined(_WIN32) || defined(_WIN64)
    typedef HMODULE  DLHANDLE;
    typedef FARPROC  DLSYMFUNC;

    #define LIBRARY_SUFFIX  ".dll"
    #define DEFAULT_DLFLAGS 0

    typedef HANDLE osal_dir_t;
#else
    typedef void*   DLHANDLE;
    typedef void*   DLSYMFUNC;
    
    #define LIBRARY_SUFFIX  ".so"
    #define DEFAULT_DLFLAGS   RTLD_LAZY

    typedef uintptr_t osal_dir_t;
#endif
/* **************************************************
    Filesystem functions
 ************************************************** */
enum {
    OSAL_DTYPE_FILE,
    OSAL_DTYPE_DIRECTORY,
    OSAL_DTYPE_UNKNOW
};

enum {
    OSAL_FACCESS_NORMAL     = 0,
    OSAL_FACCESS_READ       = 1 << 0,
    OSAL_FACCESS_WRITE      = 1 << 1
};

#if defined(_WIN32) || defined(_WIN64)
    typedef HANDLE osal_dir_t;
#else
    typedef uintptr_t osal_dir_t;
#endif

typedef struct {
    char*     name;
    uint8_t   type;

#if defined(_WIN32) || defined(_WIN64)
    WIN32_FIND_DATA __sysdata;
#else    
    struct dirent* __sysdata;
#endif
} osal_dirent_t;

