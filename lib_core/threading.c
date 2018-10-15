/*POSIX thread api wrapper
  based on deadbeef/threading_pthread by Alexey Yakovenko */

#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "api.h"
#include "threading.h"
#include "defs.h"

intptr_t
thread_create (void  (*func)(void* ctx),
               void* ctx)
{
    pthread_t tid;
    pthread_attr_t attr;
    int fail=pthread_attr_init (&attr);
    if (fail) {
        ucm_etrace("%s. %s\n","Failed in pthread_attr_init", strerror(fail));
        return 0;
    }
    fail = pthread_create(&tid, &attr, (void*(*)(void*))func, (void*)ctx);
    if (fail){
        ucm_etrace("%s. %s\n","Failed in pthread_create", strerror(fail));
        return 0;
    }
    fail = pthread_attr_destroy(&attr);
    if (fail){
        ucm_etrace("%s. %s\n","Failed in pthread_attr_destroy", strerror(fail));
        return 0;
    }
    return (intptr_t)tid;
}


int
thread_detach (intptr_t tid)
{
    int fail=pthread_detach((pthread_t)tid);
    if (fail) {
        ucm_etrace("%s. %s\n","Failed in pthread_detach", strerror(fail));
        return -1;
    }
    return 0;
}

void
thread_exit (void* ret)
{
    pthread_exit(ret);
    return;
}

int
thread_join (intptr_t tid)
{
    void* ret;
    int fail=pthread_join((pthread_t)tid,&ret);
    if (fail) {
        ucm_etrace("%s. %s\n","Failed in pthread_join", strerror(fail));
        return -1;
    }
    return 0;
}


uintptr_t
mutex_create_nonrecursive (void)
{
    pthread_mutex_t* mtx= ucm_kmalloc(sizeof(pthread_mutex_t));
    if (mtx) {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_TIMED_NP);
#endif
        int fail=pthread_mutex_init(mtx,&attr);
        if (fail) {
            ucm_etrace("%s. %s\n","Failed in pthread_mutex_init", strerror(fail));
            ucm_kfree_null(mtx);
        }
        pthread_mutexattr_destroy (&attr);
    }
    return (uintptr_t)mtx;
}

uintptr_t
mutex_create (void)
{
    pthread_mutex_t* mtx= ucm_kmalloc(sizeof(pthread_mutex_t));
    if (mtx) {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE_NP);
#endif
        int fail=pthread_mutex_init(mtx,&attr);
        if (fail) {
            ucm_etrace("%s. %s\n","Failed in pthread_mutex_init", strerror(fail));
            ucm_kfree_null(mtx);
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
        ucm_kfree(mtx);
    }
}

int
mutex_lock(uintptr_t _mtx)
{
    pthread_mutex_t* mtx = (pthread_mutex_t*)_mtx;
    int fail = pthread_mutex_lock(mtx);
    if (fail)
        ucm_etrace("%s. %s\n","Failed in pthread_mutex_lock", strerror(fail));
    return fail;
}

int
mutex_unlock(uintptr_t _mtx)
{
    pthread_mutex_t* mtx = (pthread_mutex_t*)_mtx;
    int fail = pthread_mutex_unlock(mtx);
    if (fail)
        ucm_etrace("%s. %s\n","Failed in pthread_mutex_unlock", strerror(fail));
    return fail;
}

uintptr_t
cond_create (void)
{
    pthread_cond_t *cond = ucm_kmalloc(sizeof(pthread_cond_t));
    if (cond) {
        int fail = pthread_cond_init(cond, NULL);
        if (fail) {
            ucm_etrace("%s. %s\n","Failed in pthread_cond_create", strerror(fail));
            ucm_kfree_null(cond);
        }
    }
    return (uintptr_t)cond;
}

void
cond_free(uintptr_t _cond)
{
    if (_cond) {
        pthread_cond_t* cond = (pthread_cond_t*)_cond;
        pthread_cond_destroy(cond);
        ucm_kfree(cond);
    }
}

int
cond_wait (uintptr_t _cond,
           uintptr_t _mtx)
{
    pthread_cond_t* cond = (pthread_cond_t*) _cond;
    pthread_mutex_t* mtx = (pthread_mutex_t*) _mtx;
    int fail = mutex_lock(_mtx);
    if (fail)
        return fail;
    fail = pthread_cond_wait(cond,mtx);
    if (fail)
        ucm_etrace("%s. %s\n","Failed in pthread_cond_wait", strerror(fail));
    return fail;
}

int
cond_signal (uintptr_t _cond)
{
    pthread_cond_t* cond = (pthread_cond_t*) _cond;
    int fail = pthread_cond_signal (cond);
    if (fail)
        ucm_etrace("%s. %s\n","Failed in pthread_cond_signal", strerror(fail));
    return fail;
}

int
cond_broadcast (uintptr_t _cond)
{
    pthread_cond_t* cond = (pthread_cond_t*) _cond;
    int fail = pthread_cond_broadcast (cond);
    if (fail)
        ucm_etrace("%s. %s\n","Failed in pthread_cond_broadcast", strerror(fail));
    return fail;
}

uintptr_t
rwlock_create (void)
{
    pthread_rwlock_t* rwl = malloc(sizeof(pthread_rwlock_t));
    if (rwl) {
        pthread_rwlockattr_t attr;
        pthread_rwlockattr_init(&attr);
#if defined (__GLIBC__)
        pthread_rwlockattr_setkind_np(&attr,PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
#endif
        int fail = pthread_rwlock_init(rwl,&attr);
        if(fail){
            ucm_etrace("%s. %s\n","Failed in rwlock_create", strerror(fail));
            ucm_kfree_null(rwl);
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
        ucm_kfree(rwl);
    }
}

int
rwlock_rlock (uintptr_t _rwl)
{
    pthread_rwlock_t* rwl = (pthread_rwlock_t*)_rwl;
    int fail = pthread_rwlock_rdlock(rwl);
    if (fail)
        ucm_etrace("%s: %s\n","Failed rwlock read lock",strerror(fail));
    return fail;
}

int
rwlock_wlock (uintptr_t _rwl)
{
    pthread_rwlock_t* rwl = (pthread_rwlock_t*)_rwl;
    int fail = pthread_rwlock_wrlock(rwl);
    if (fail)
        ucm_etrace("%s: %s\n","Failed rwlock write lock",strerror(fail));
    return fail;
}

int
rwlock_unlock(uintptr_t _rwl)
{
    pthread_rwlock_t* rwl = (pthread_rwlock_t*)_rwl;
    int fail = pthread_rwlock_unlock(rwl);
    if (fail)
        ucm_etrace("%s: %s\n","Failed rwlock write lock",strerror(fail));
    return fail;
}
