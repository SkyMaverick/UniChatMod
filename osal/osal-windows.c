#include "osal.h"

uintptr_t
osal_thread_create ( THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
                void* ctx)
{
    return (uintptr_t) CreateThread (NULL, 0, func, ctx, 0, NULL);
}

int
osal_thread_detach (uintptr_t tid)
{
    return 0;
}

void
osal_thread_exit (THREAD_RESULT ret)
{
    ExitThread (ret);
}

int
osal_thread_join (uintptr_t tid)
{
    return WaitForSingleObject( (HANDLE)tid, INFINITE);
}


uintptr_t
osal_mutex_create_nonrecursive (void)
{
    return 0;
}

uintptr_t
osal_mutex_create (void)
{
    return 0;
}

void
osal_mutex_free (uintptr_t _mtx)
{
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
osal_cond_lock(uintptr_t _cond)
{
    return 0;
}

int
osal_cond_unlock (uintptr_t _cond)
{
    return 0;
}

void
osal_cond_free(uintptr_t _cond)
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

osal_dir_t
osal_diropen (const char*       path,
             osal_fsobject_t*   fso)
{
    return 0;
}

int
osal_dirnext (osal_dir_t        dir,
             osal_fsobject_t*  fso)
{
    return 0;
}

void
osal_dirclose (osal_dir_t fso) {

}
