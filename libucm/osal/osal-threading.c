#include "osal.h"

enum {
    OSAL_RWLMODE_READ  = 0,
    OSAL_RWLMODE_WRITE = 1
};

typedef struct {
    PRWLock* lock;
    int mode;
} osal_rwlock_t;

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
int
osal_thread_exit (void)
{
    int ret = 0;
    p_uthread_exit (ret);
    return ret;
}
int
osal_thread_join (uintptr_t tid)
{
    return p_uthread_join ( (PUThread*)tid );
}
void
osal_thread_cleanup (uintptr_t* tid)
{
    p_uthread_unref( (PUThread*)tid );
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
bool
osal_mutex_lock (uintptr_t _mtx)
{
    return p_mutex_lock( (PMutex*)_mtx );
}
bool
osal_mutex_unlock (uintptr_t _mtx)
{
    return p_mutex_unlock( (PMutex*)_mtx );
}

uintptr_t
osal_cond_create (void)
{
    return (uintptr_t)p_cond_variable_new();
}
void
osal_cond_free (uintptr_t _cond)
{
    return p_cond_variable_free ((PCondVariable*)_cond);
}
bool
osal_cond_wait (uintptr_t _cond,
                uintptr_t _mtx)
{
    return p_cond_variable_wait((PCondVariable*)_cond,
                                (PMutex*)_mtx);
}
bool
osal_cond_signal (uintptr_t _cond)
{
    return p_cond_variable_signal((PCondVariable*)_cond);
}
bool
osal_cond_broadcast (uintptr_t _cond)
{
    return p_cond_variable_broadcast((PCondVariable*)_cond);
}

uintptr_t
osal_rwlock_create (void)
{
    osal_rwlock_t* ret = osal_zmalloc(sizeof(osal_rwlock_t));
    if ( P_LIKELY(ret) ) {
        ret->lock = p_rwlock_new();
        if ( P_UNLIKELY(ret->lock == NULL) )
            osal_free_null (ret);
    }
    return (uintptr_t)ret;
}
void
osal_rwlock_free (uintptr_t _rwl)
{
    p_rwlock_free ( ((osal_rwlock_t*)_rwl)->lock );
    osal_free ((osal_rwlock_t*)_rwl);
}
bool
osal_rwlock_rlock (uintptr_t _rwl)
{

    bool ret = p_rwlock_reader_lock( ((osal_rwlock_t*)_rwl)->lock );
    if (ret)
        ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_READ;
    return ret;    
}
bool
osal_rwlock_wlock (uintptr_t _rwl)
{
    bool ret = p_rwlock_writer_lock( ((osal_rwlock_t*)_rwl)->lock );
    if (ret)
        ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_WRITE;
    return ret;    
}
bool
osal_rwlock_unlock (uintptr_t _rwl)
{
    bool ret = false;
    switch (((osal_rwlock_t*)_rwl)->mode){
        case OSAL_RWLMODE_READ:
            {
                ret = p_rwlock_reader_unlock(((osal_rwlock_t*)_rwl)->lock);
                break;
            };
        case OSAL_RWLMODE_WRITE:
            {
                ret = p_rwlock_writer_unlock(((osal_rwlock_t*)_rwl)->lock);
                break;
            };
    }
    return ret;
}

