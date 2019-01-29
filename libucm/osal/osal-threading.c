#include "osal.h"

uintptr_t
osal_thread_create (void* (*func)(void* ctx),
                    void* ctx)
{
    PUThread* ret  = p_uthread_create (func, ctx, TRUE);
    if (ret)
        p_uthread_ref (ret);
    return (uintptr_t) ret;
}
int
osal_thread_detach (uintptr_t tid)
{
    return 0;
}
void
osal_thread_exit (void* ret)
{
}
int
osal_thread_join (uintptr_t tid)
{
    int ret = p_uthread_join ( (PUThread*)tid );
    if (ret == 0)
        p_uthread_unref( (PUThread*)tid );
    return ret;
}
void
osal_thread_cleanup (uintptr_t* tid)
{
}

uintptr_t
osal_mutex_create (void)
{
    return (uintptr_t)p_mutex_new();
}
void
osal_mutex_free (uintptr_t _mtx)
{
    p_mutex_free( (PMutex*)_mtx );
}
int
osal_mutex_lock (uintptr_t _mtx)
{
    return 0;
}
int
osal_mutex_unlock (uintptr_t _mtx)
{
    return 0;
}

uintptr_t
osal_cond_create (void)
{
    return 0;
}
int
osal_cond_lock (uintptr_t _cond)
{
    return 0;
}
int
osal_cond_unlock (uintptr_t _cond)
{
    return 0;
}
void
osal_cond_free (uintptr_t _cond)
{
}
int
osal_cond_wait (uintptr_t _cond)
{
    return 0;
}
int
osal_cond_signal (uintptr_t _cond)
{
    return 0;
}
int
osal_cond_broadcast (uintptr_t _cond)
{
    return 0;
}

uintptr_t
osal_rwlock_create (void)
{
    return 0;
}
void
osal_rwlock_free (uintptr_t _rwl)
{
}
int
osal_rwlock_rlock (uintptr_t _rwl)
{
    return 0;
}
int
osal_rwlock_wlock (uintptr_t _rwl)
{
    return 0;
}
int
osal_rwlock_unlock (uintptr_t _rwl)
{
    return 0;
}

