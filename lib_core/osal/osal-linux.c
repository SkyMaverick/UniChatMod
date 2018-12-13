#include "osal.h"

/*POSIX thread api wrapper
  based on deadbeef/threading_pthread by Alexey Yakovenko */

#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t  cond;
} ucm_cmutex_t;

uintptr_t thread_create (
        THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
        void* ctx)
{
    pthread_t tid;
    pthread_attr_t attr;
    
    pthread_attr_init (&attr);
    pthread_create(&tid, &attr, func, ctx);
    pthread_attr_destroy(&attr);
    
    return (uintptr_t)tid;
}


inline int
thread_detach (uintptr_t tid)
{
    return pthread_detach((pthread_t)tid) ? 1 : 0;
}

inline void
thread_exit (void* ret)
{
    pthread_exit(ret);
}

int
thread_join (uintptr_t tid)
{
    void* ret;
    return pthread_join((pthread_t)tid,&ret) ? 1 : 0;
}


uintptr_t
mutex_create_nonrecursive (void)
{
    pthread_mutex_t* mtx= ucm_malloc(sizeof(pthread_mutex_t));
    if (mtx) {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_TIMED_NP);
#endif
        if ( pthread_mutex_init(mtx,&attr) ) {
            ucm_free (mtx);
            mtx = NULL;
        }
        pthread_mutexattr_destroy (&attr);
    }
    return (uintptr_t)mtx;
}

uintptr_t
mutex_create (void)
{
    pthread_mutex_t* mtx= ucm_malloc(sizeof(pthread_mutex_t));
    if (mtx) {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE_NP);
#endif
        if ( pthread_mutex_init(mtx,&attr) ) {
            ucm_free (mtx);
            mtx = NULL;
        }
        pthread_mutexattr_destroy (&attr);
    }
    return (uintptr_t)mtx;
}

void
mutex_free (uintptr_t _mtx)
{
    if (_mtx) {
        pthread_mutex_t* mtx = (pthread_mutex_t*)_mtx;
        pthread_mutex_destroy(mtx);
        ucm_free(mtx);
    }
}

inline int
mutex_lock(uintptr_t _mtx)
{
    return pthread_mutex_lock ( (pthread_mutex_t*)_mtx );
}

inline int
mutex_unlock(uintptr_t _mtx)
{
    return pthread_mutex_unlock ( (pthread_mutex_t*)_mtx );
}

uintptr_t
cond_create (void)
{
    ucm_cmutex_t *cmtx = ucm_malloc(sizeof(ucm_cmutex_t));
    if (cmtx) {
        if ( pthread_cond_init(&(cmtx->cond), NULL) ) {
            ucm_free (cmtx);
            cmtx = NULL;
            goto exit;
        };

        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE_NP);
#endif
        if ( pthread_mutex_init (&(cmtx->mtx),&attr) ) {
            ucm_free (cmtx);
            cmtx = NULL;
        }
        pthread_mutexattr_destroy (&attr);
    }
    exit:return (uintptr_t)cmtx;
}

inline int
cond_lock (uintptr_t _cond)
{
    return mutex_lock ( (uintptr_t)(&(((ucm_cmutex_t*)_cond)->mtx)) );
}

inline int
cond_unlock (uintptr_t _cond)
{
    return mutex_unlock ( (uintptr_t)(&(((ucm_cmutex_t*)_cond)->mtx)) );
}

void
cond_free(uintptr_t _cond)
{
    if (_cond) {
        ucm_cmutex_t* cmtx = (ucm_cmutex_t*)_cond;
        pthread_cond_destroy ( &(cmtx->cond) );
        pthread_mutex_destroy ( &(cmtx->mtx) );
        ucm_free (cmtx);
    }
}

int
cond_wait (uintptr_t _cond)
{
    ucm_cmutex_t* cmtx = (ucm_cmutex_t*) _cond;
    int fail = mutex_lock ( (uintptr_t)(&(cmtx->mtx)) );
    
    return fail ? fail : pthread_cond_wait (&(cmtx->cond),
                                            &(cmtx->mtx));
}

inline int
cond_signal (uintptr_t _cond)
{
    return pthread_cond_signal (&(((ucm_cmutex_t*) _cond)->cond));
}

inline int
cond_broadcast (uintptr_t _cond)
{
    return pthread_cond_broadcast ( &( ((ucm_cmutex_t*)_cond)->cond ) );
}

uintptr_t
rwlock_create (void)
{
    pthread_rwlock_t* rwl = ucm_malloc(sizeof(pthread_rwlock_t));
    if (rwl) {
        pthread_rwlockattr_t attr;
        pthread_rwlockattr_init(&attr);
#if defined (__GLIBC__)
        pthread_rwlockattr_setkind_np(&attr,PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
#endif
        if ( pthread_rwlock_init(rwl,&attr) ) {
            ucm_free (rwl);
            rwl = NULL;
        }
        pthread_rwlockattr_destroy(&attr);
    }
    return (uintptr_t)rwl;
}

void
rwlock_free (uintptr_t _rwl)
{
    if (_rwl) {
        pthread_rwlock_t* rwl = (pthread_rwlock_t*)_rwl;
        pthread_rwlock_destroy(rwl);
        ucm_free(rwl);
    }
}

inline int
rwlock_rlock (uintptr_t _rwl)
{
    return pthread_rwlock_rdlock ((pthread_rwlock_t*)_rwl);
}

inline int
rwlock_wlock (uintptr_t _rwl)
{
    return pthread_rwlock_wrlock ((pthread_rwlock_t*)_rwl);
}

inline int
rwlock_unlock(uintptr_t _rwl)
{
    return pthread_rwlock_unlock ((pthread_rwlock_t*)_rwl);
}