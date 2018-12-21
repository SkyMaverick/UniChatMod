#include "osal.h"

inline uintptr_t
thread_create ( THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
                void* ctx)
{
    return (uintptr_t) CreateThread (NULL, 0, func, ctx, 0, NULL);
}

int thread_detach (uintptr_t tid)
{
    return 0;
}

inline void
thread_exit (THREAD_RESULT ret)
{
    ExitThread (THREAD_RESULT);
}

inline int
thread_join (uintptr_t tid)
{
    WaitForSingleObject( (HANDLE)tid, infinite);
}


uintptr_t mutex_create_nonrecursive(void)
{
    return 0;
}

uintptr_t mutex_create (void)
{
    return 0;
}

void mutex_free (uintptr_t _mtx)
{
    return 0;
}

int mutex_lock (uintptr_t _mtx)
{
    return 0;
}

int mutex_unlock (uintptr_t _mtx)
{
    return 0;
}


uintptr_t cond_create (void)
{
    return 0;
}

int cond_lock(uintptr_t _cond)
{
    return 0;
}

int cond_unlock (uintptr_t _cond)
{
    return 0;
}

void cond_free(uintptr_t _cond)
{
    return 0;
}

int cond_wait (uintptr_t _cond)
{
    return 0;
}

int cond_signal (uintptr_t _cond)
{
    return 0;
}

int cond_broadcast (uintptr_t _cond)
{
    return 0;
}


uintptr_t rwlock_create (void)
{
    return 0;
}

void rwlock_free (uintptr_t _rwl)
{
    return 0;
}

int rwlock_rlock (uintptr_t _rwl)
{
    return 0;
}

int rwlock_wlock (uintptr_t _rwl)
{
    return 0;
}

int rwlock_unlock (uintptr_t _rwl)
{
    return 0;
}
