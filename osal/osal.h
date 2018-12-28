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
    #ifdef osal_WITHOUT_RUNTIME
        #ifndef osal_malloc
            static inline void*
            osal_malloc (size_t bytes)
            {
                return LocalAlloc (LMEM_FIXED, bytes);
            }
        #endif
        #ifndef osal_calloc
            static inline void*
            osal_calloc (size_t nmemb,
                        size_t bytes)
            {
                return LocalAlloc (LMEM_FIXED | LMEM_ZEROINIT, bytes);
            }
        #endif
        #ifndef osal_free
            #define osal_free LocalFree
        #endif
        #ifndef osal_realloc
            static inline void*
            osal_realloc (void*  ptr,
                         size_t bytes)
            {
                return LocalReAlloc (ptr, bytes, LMEM_MOVEABLE);
            }
        #endif
    #else
        #define osal_malloc  malloc
        #define osal_calloc  calloc
        #define osal_free    free
        #define osal_realloc realloc
        #define osal_strdup  _strdup
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

    #define osal_malloc  malloc
    #define osal_calloc  calloc
    #define osal_free    free
    #define osal_realloc realloc
    #ifdef strdup
        #define osal_strdup  strdup
    #endif
#endif

static inline void*
osal_zmalloc (size_t bytes)
{
    return osal_calloc (1, bytes);
}

static inline void
osal_zmemory (void*  ptr,
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

static inline int
osal_realloc2 (void** mem, size_t size)
{
    void* old_mem = *mem;
    *mem = osal_realloc (*mem, size);
    if (*mem != old_mem) {
        if (*mem != NULL) {
            osal_free (old_mem);
        } else {
            *mem = old_mem;
        }
        return 1;
    }
    return 0;
}

/* ======================================================================
        CUSTOM DYNAMIC LOAD LIBRARIES FUNCTIONS
   ====================================================================== */
#if defined(_WIN32) || defined(_WIN64)
    typedef HMODULE  DLHANDLE;
    typedef FARPROC  DLSYMFUNC;
    #ifndef osal_dlopen
        static inline DLHANDLE
        osal_dlopen (char* path, int mode)
        {
            return LoadLibraryExA (path, NULL, (DWORD)mode);
        }
    #endif
    #ifndef osal_dlclose
        static inline void
        osal_dlclose (DLHANDLE hndl)
        {
            FreeLibrary (hndl);
        }
    #endif
    #ifndef osal_dlsym
        static inline DLSYMFUNC
        osal_dlsym (DLHANDLE lib, const char* func)
        {
            return GetProcAddress (lib, func);
        }
    #endif
    #ifndef osal_dlerror
        static inline char*
        osal_dlerror () {
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
    #define osal_dlopen     dlopen
    #define osal_dlclose    dlclose
    #define osal_dlsym      dlsym
    #define osal_dlerror    dlerror

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

uintptr_t osal_thread_create (
        THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
        void* ctx);
int osal_thread_detach       (uintptr_t tid);
void osal_thread_exit        (THREAD_RESULT ret);
int osal_thread_join         (uintptr_t tid);

uintptr_t osal_mutex_create_nonrecursive(void);
uintptr_t osal_mutex_create  (void);
void osal_mutex_free         (uintptr_t _mtx);
int osal_mutex_lock          (uintptr_t _mtx);
int osal_mutex_unlock        (uintptr_t _mtx);

uintptr_t osal_cond_create   (void) ;
int osal_cond_lock           (uintptr_t _cond);
int osal_cond_unlock         (uintptr_t _cond);
void osal_cond_free          (uintptr_t _cond);
int osal_cond_wait           (uintptr_t _cond);
int osal_cond_signal         (uintptr_t _cond);
int osal_cond_broadcast      (uintptr_t _cond);

uintptr_t osal_rwlock_create (void);
void osal_rwlock_free        (uintptr_t _rwl);
int osal_rwlock_rlock        (uintptr_t _rwl);
int osal_rwlock_wlock        (uintptr_t _rwl);
int osal_rwlock_unlock       (uintptr_t _rwl);

/* ======================================================================
        CUSTOM FUNCTIONS
   ====================================================================== */
static inline int
osal_errno (void)
{
#if defined(_WIN32) || defined(_WIN64)
    return (int) GetLastError();
#else
    return errno;
#endif
}

#ifndef osal_strdup
char*
osal_strdup (const char* str);
#endif

#if defined(_WIN32) || defined(_WIN64)
    typedef HANDLE osal_dir_t;
#else
    typedef struct {
        DIR* handle;
        char path [1];
    } posix_dir_t;

    typedef uintptr_t osal_dir_t;
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
} osal_fsobject_t;

osal_dir_t
osal_diropen (const char*       path,
             osal_fsobject_t*   fso);

int
osal_dirnext (osal_dir_t        dir,
             osal_fsobject_t*  fso);

void
osal_dirclose (osal_dir_t fso);

#ifdef __cplusplus
    }
#endif
