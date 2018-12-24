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


#ifdef __cplusplus
    extern "C" {
#endif

#ifndef _POSIX_C_SOURCE
    #ifdef _POSIX_SOURCE
        #define _POSIX_C_SOURCE 1
    #else
        #define _POSIX_C_SOURCE 0
    #endif
#endif

#ifndef _XOPEN_SOURCE
    #define _XOPEN_SOURCE 0
#endif

/* ======================================================================
        CUSTOM MEMORY ALLOCATION INLINE FUNCTIONS 
   ====================================================================== */

#if defined(_WIN32) || defined(_WIN64)

/*
    Windows specified includes
 */

    #include <Windows.h>
    #include <WinNT.h>
    #include <Winternl.h>
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
    #include <sys/select.h>
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
    #ifdef strdup
        #define ucm_strdup  strdup
    #endif
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
#if defined(_WIN32) || defined(_WIN64)
    typedef HMODULE  DLHANDLE;
    typedef FARPROC  DLSYMFUNC;
    #ifndef ucm_dlopen
        static inline DLHANDLE
        ucm_dlopen (char* path, int mode)
        {
            return LoadLibraryExA (path, NULL, (DWORD)mode);
        }
    #endif
    #ifndef ucm_dlclose
        static inline void
        ucm_dlclose (DLHANDLE hndl)
        {
            FreeLibrary (hndl);
        }
    #endif
    #ifndef ucm_dlsym
        static inline DLSYMFUNC
        ucm_dlsym (DLHANDLE lib, const char* func)
        {
            return GetProcAddress (lib, func);
        }
    #endif
    #ifndef ucm_dlerror
        static inline char*
        ucm_dlerror () {
            char* buffer = NULL;
            FormatMessage ( FORMAT_MESSAGE_ALLOCATE_BUFFER |
                            FORMAT_MESSAGE_FROM_SYSTEM |
                            FORMAT_MESSAGE_IGNORE_INSERTS,
                            NULL, GetLastError(),
                            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                            (LPTSTR) buffer, 0, NULL);
            return buffer;
        }
    #endif
    #define LIBRARY_SUFFIX  ".dll"
    #define DEFAULT_DLFLAGS 0
#else
    typedef void*   DLHANDLE;
    typedef void*   DLSYMFUNC;
    #define ucm_dlopen     dlopen
    #define ucm_dlclose    dlclose
    #define ucm_dlsym      dlsym
    #define ucm_dlerror    dlerror

    #define LIBRARY_SUFFIX  ".so"
    #define DEFAULT_DLFLAGS   RTLD_LAZY
#endif
/* ======================================================================
        CUSTOM THREADING FUNCTIONS
   ====================================================================== */

#if defined(_WIN32) || defined(_WIN64)
    #define THREAD_CALL    __stdcall
    #define THREAD_RESULT  unsigned long
#else
    #define THREAD_CALL
    #define THREAD_RESULT  void *
#endif

uintptr_t ucm_thread_create (
        THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
        void* ctx);
int ucm_thread_detach       (uintptr_t tid);
void ucm_thread_exit        (THREAD_RESULT ret);
int ucm_thread_join         (uintptr_t tid);

uintptr_t ucm_mutex_create_nonrecursive(void);
uintptr_t ucm_mutex_create  (void);
void ucm_mutex_free         (uintptr_t _mtx);
int ucm_mutex_lock          (uintptr_t _mtx);
int ucm_mutex_unlock        (uintptr_t _mtx);

uintptr_t ucm_cond_create   (void) ;
int ucm_cond_lock           (uintptr_t _cond);
int ucm_cond_unlock         (uintptr_t _cond);
void ucm_cond_free          (uintptr_t _cond);
int ucm_cond_wait           (uintptr_t _cond);
int ucm_cond_signal         (uintptr_t _cond);
int ucm_cond_broadcast      (uintptr_t _cond);

uintptr_t ucm_rwlock_create (void);
void ucm_rwlock_free        (uintptr_t _rwl);
int ucm_rwlock_rlock        (uintptr_t _rwl);
int ucm_rwlock_wlock        (uintptr_t _rwl);
int ucm_rwlock_unlock       (uintptr_t _rwl);

/* ======================================================================
        CUSTOM FUNCTIONS
   ====================================================================== */
static inline int
ucm_errno (void)
{
#if defined(_WIN32) || defined(_WIN64)
    return (int) GetLastError();
#else
    return errno;
#endif
}

#ifndef ucm_strdup
char*
ucm_strdup (const char* str);
#endif

#if defined(_WIN32) || defined(_WIN64)
    typedef HANDLE ucm_dir_t;
#else
    typedef struct {
        DIR* handle;
        char path [1];
    } posix_dir_t;

    typedef uintptr_t ucm_dir_t;
#endif

enum {
    FO_TYPE_FILE,
    FO_TYPE_DIRECTORY,
    FO_TYPE_UNKNOW
};

typedef struct {
    char*     name;
    uint8_t   type;

    uintptr_t handle;
} ucm_fsobject_t;

ucm_dir_t
ucm_diropen (const char*       path,
             ucm_fsobject_t*   fso);

int
ucm_dirnext (ucm_dir_t        dir,
             ucm_fsobject_t*  fso);

void
ucm_dirclose (ucm_dir_t fso);

#ifdef __cplusplus
    }
#endif
