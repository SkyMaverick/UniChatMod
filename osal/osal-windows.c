#include "osal.h"

uintptr_t
ucm_thread_create ( THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
                void* ctx)
{
    return (uintptr_t) CreateThread (NULL, 0, func, ctx, 0, NULL);
}

int
ucm_thread_detach (uintptr_t tid)
{
    return 0;
}

void
ucm_thread_exit (THREAD_RESULT ret)
{
    ExitThread (ret);
}

int
ucm_thread_join (uintptr_t tid)
{
    return WaitForSingleObject( (HANDLE)tid, INFINITE);
}


uintptr_t
ucm_mutex_create_nonrecursive (void)
{
    return 0;
}

uintptr_t
ucm_mutex_create (void)
{
    return 0;
}

void
ucm_mutex_free (uintptr_t _mtx)
{
}

int
ucm_mutex_lock (uintptr_t _mtx)
{
    return 0;
}

int
ucm_mutex_unlock (uintptr_t _mtx)
{
    return 0;
}


uintptr_t
ucm_cond_create (void)
{
    return 0;
}

int
ucm_cond_lock(uintptr_t _cond)
{
    return 0;
}

int
ucm_cond_unlock (uintptr_t _cond)
{
    return 0;
}

void
ucm_cond_free(uintptr_t _cond)
{
}

int
ucm_cond_wait (uintptr_t _cond)
{
    return 0;
}

int
ucm_cond_signal (uintptr_t _cond)
{
    return 0;
}

int
ucm_cond_broadcast (uintptr_t _cond)
{
    return 0;
}

uintptr_t
ucm_rwlock_create (void)
{
    return 0;
}

void
ucm_rwlock_free (uintptr_t _rwl)
{
}

int
ucm_rwlock_rlock (uintptr_t _rwl)
{
    return 0;
}

int
ucm_rwlock_wlock (uintptr_t _rwl)
{
    return 0;
}

int
ucm_rwlock_unlock (uintptr_t _rwl)
{
    return 0;
}

ucm_dir_t
ucm_diropen (const char*       path,
             ucm_fsobject_t*   fso)
{
    return 0;
}

int
ucm_dirnext (ucm_dir_t        dir,
             ucm_fsobject_t*  fso)
{
    return 0;
}

void
ucm_dirclose (ucm_dir_t fso) {

}
