#include "osal-intrnl.h"

enum {
    OSAL_RWLMODE_IDLE  = 0,
    OSAL_RWLMODE_READ  = 1,
    OSAL_RWLMODE_WRITE = 2
};

typedef struct {
    uv_rwlock_t lock;
    int mode;
} osal_rwlock_t;

uintptr_t
osal_thread_create (void* (*func)(void* ctx),
                    void* ctx)
{
    uv_thread_t* tid = osal_malloc (sizeof(uv_thread_t));
    if ( __likely(tid) ){
        if ( __likely (uv_thread_create (tid, (uv_thread_cb)func, ctx) == 0) ) {
            return (uintptr_t)tid;
        } else {
            osal_free (tid);
        }
    }
    return 0;
}

int
osal_thread_detach (uintptr_t tid)
{
    // TODO
    return 0;
}

void*
osal_thread_exit (void)
{
    void* ret = NULL;
#if defined (UCM_OS_WINDOWS)
    _endthreadex(0);
#elif defined (UCM_OS_POSIX)
    pthread_exit(ret);
#else
    #warning "Don't define for this platform. Create dummy function"
#endif
    return ret;
}
int
osal_thread_join (uintptr_t tid)
{
    return uv_thread_join ( (uv_thread_t*)tid );
}
void
osal_thread_cleanup (uintptr_t* tid)
{
    uv_thread_t* tmp = (uv_thread_t*)(*tid);
    osal_free_null (tmp);
}

uintptr_t
osal_mutex_create (void)
{
    uv_mutex_t* mtx = osal_malloc (sizeof(uv_mutex_t));
    if ( __likely(mtx) ) {
        if ( __likely (uv_mutex_init (mtx) == 0) ) {
            return (uintptr_t) mtx;
        } else {
            osal_free(mtx);
        }
    } 
    return 0;
}
void
osal_mutex_free (uintptr_t _mtx)
{
    uv_mutex_destroy ( (uv_mutex_t*)_mtx );
    osal_free ((uv_mutex_t*)_mtx);
}
void
osal_mutex_lock (uintptr_t _mtx)
{
    uv_mutex_lock( (uv_mutex_t*)_mtx );
}
int
osal_mutex_trylock (uintptr_t _mtx) {
    return uv_mutex_trylock( (uv_mutex_t*)_mtx );
}
void
osal_mutex_unlock (uintptr_t _mtx)
{
    uv_mutex_unlock ( (uv_mutex_t*)_mtx );
}

uintptr_t
osal_cond_create (void)
{
    uv_cond_t* cond = osal_malloc (sizeof(uv_cond_t));
    if ( __likely(cond) ) {
        if ( __likely (uv_cond_init(cond) == 0) ){
            return (uintptr_t)cond;
        } else {
            osal_free (cond);
        }
    }
    return 0;
}
void
osal_cond_free (uintptr_t _cond)
{
    uv_cond_destroy ((uv_cond_t*)_cond);
    osal_free ((uv_cond_t*)_cond);
}
void
osal_cond_wait (uintptr_t _cond,
                uintptr_t _mtx)
{
    uv_cond_wait( (uv_cond_t*)_cond,
                  (uv_mutex_t*)_mtx );
}
void
osal_cond_signal (uintptr_t _cond)
{
    uv_cond_signal ((uv_cond_t*)_cond);
}
void
osal_cond_broadcast (uintptr_t _cond)
{
    uv_cond_broadcast((uv_cond_t*)_cond);
}

uintptr_t
osal_rwlock_create (void)
{
    osal_rwlock_t* rwl = osal_zmalloc(sizeof(osal_rwlock_t));
    if ( __likely(rwl) ) {
        if ( __likely (uv_rwlock_init(&(rwl->lock)) == 0) ) {
            return (uintptr_t)rwl;
        } else {
            osal_free (rwl);
        }
    }
    return 0;
}
void
osal_rwlock_free (uintptr_t _rwl)
{
    uv_rwlock_destroy ( (uv_rwlock_t*)_rwl);
    osal_free ((osal_rwlock_t*)_rwl);
}
void
osal_rwlock_rlock (uintptr_t _rwl)
{

    uv_rwlock_rdlock ( (uv_rwlock_t*)_rwl );
    ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_READ;
}
void
osal_rwlock_wlock (uintptr_t _rwl)
{
    uv_rwlock_wrlock ( (uv_rwlock_t*)_rwl );
    ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_WRITE;
}
int
osal_rwlock_tryrlock (uintptr_t _rwl)
{

    int ret = uv_rwlock_tryrdlock ( (uv_rwlock_t*)_rwl );
    if (ret == 0)
        ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_READ;
    return ret;
}
int
osal_rwlock_trywlock (uintptr_t _rwl)
{
    int ret = uv_rwlock_trywrlock ( (uv_rwlock_t*)_rwl );
    if (ret == 0 )
        ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_WRITE;
    return ret;
}
void
osal_rwlock_unlock (uintptr_t _rwl)
{
    switch (((osal_rwlock_t*)_rwl)->mode){
        case OSAL_RWLMODE_IDLE: return;
        case OSAL_RWLMODE_READ:
            {
                ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_IDLE;
                uv_rwlock_rdunlock ( (uv_rwlock_t*)_rwl );
                break;
            };
        case OSAL_RWLMODE_WRITE:
            {
                ((osal_rwlock_t*)_rwl)->mode = OSAL_RWLMODE_IDLE;
                uv_rwlock_wrunlock ( (uv_rwlock_t*)_rwl ); break;
            };
    }
}
