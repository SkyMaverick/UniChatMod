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

#ifndef _POSIX_C_SOURCE
    #ifdef _POSIX_SOURCE
        #define _POSIX_C_SOURCE 1
    #else
        #define _POSIX_C_SOURCE 0
    #endif
#endif

/* ======================================================================
        CUSTOM MEMORY ALLOCATION FUNCTIONS
   ====================================================================== */

#if defined(_WIN32) || defined(_WIN64)

/*
    Windows specified includes
 */

    #include <windows.h>
    #include <winnt.h>
    #include <winternl.h>
    #ifdef UCM_WITHOUT_RUNTIME
        #ifndef ucm_malloc 
            static inline void* 
            ucm_malloc (size_t bytes)
            {
                return LocalAlloc (LMEM_FIXED, bytes);
            }
        #endif
        #ifndef ucm_calloc
            static inline void*
            ucm_calloc (size_t nmemb,
                        size_t bytes)
            {
                return LocalAlloc (LMEM_FIXED | LMEM_ZEROINIT, bytes);
            }
        #endif
        #ifndef ucm_free
            #define ucm_free LocalFree
        #endif
        #ifndef ucm_realloc
            static inline void*
            ucm_realloc (void*  ptr,
                         size_t bytes)
            {
                return LocalReAlloc (ptr, bytes, LMEM_MOVEABLE);
            }
        #endif
    #else
        #define ucm_malloc  malloc
        #define ucm_calloc  calloc
        #define ucm_free    free
        #define ucm_realloc realloc
        #define ucm_strdup  _strdup
    #endif
#else
    #include <pthread.h>
    #include <signal.h>
    #include <sys/file.h>
    #include <sys/stat.h>
    #include <sys/types.h>
    #include <unistd.h>
    #include <fcntl.h>
    #include <dlfcn.h>
    #include <errno.h>
    #include <dirent.h>
    #include <malloc.h>

    #define ucm_malloc  malloc
    #define ucm_calloc  calloc
    #define ucm_free    free
    #define ucm_realloc realloc
    #define ucm_strdup  strdup
#endif

static inline void*
ucm_zmalloc (size_t bytes)
{
    return ucm_calloc (1, bytes);
}

static inline void
ucm_zmemory (void*  ptr,
             size_t bytes)
{
    if (bytes) {
        if ( bytes > 4096 ) {
#if defined(_WIN32) || defined(_WIN64)
            ZeroMemory (ptr, bytes);
#else
            memset (ptr, 0, bytes);
#endif
        } else {
            do {
                bytes--;
                ((char*)ptr)[bytes] = 0;
            } while (bytes);
        }
    }
}

/* ======================================================================
        CUSTOM DYNAMIC LOAD LIBRARIES FUNCTIONS
   ====================================================================== */

