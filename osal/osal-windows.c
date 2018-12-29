#include "osal.h"

typedef struct {
    HANDLE  mutex;
    HANDLE  event;
} osal_cmutex_t;

enum {
    SRWLOCK_METHOD_NONE,
    SRWLOCK_METHOD_SHARED,
    SRWLOCK_METHOD_EXCLUSIVE
};

typedef struct {
    PSRWLOCK handle;
    int lock_method;
} osal_rwmutex_t;

static int waitret (DWORD result) {
  switch (result) {
  case WAIT_OBJECT_0:
    return OSAL_RETURN_SUCCESS;
  case WAIT_FAILED:
    return osal_errno();
  case WAIT_ABANDONED:
    return OSAL_RETURN_EABANDONED;;
  case WAIT_IO_COMPLETION:
    return OSAL_RETURN_EIO;
  case WAIT_TIMEOUT:
    return OSAL_RETURN_ETIMEOUT;
  default:
    return OSAL_RETURN_ERROR;
  }
}
uintptr_t
osal_thread_create ( THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
                void* ctx)
{
    return (uintptr_t) CreateThread (NULL, 0, func, ctx, 0, NULL);
}

int
osal_thread_detach (uintptr_t tid)
{
    return OSAL_RETURN_SUCCESS;
}

void
osal_thread_exit (THREAD_RESULT ret)
{
    ExitThread (ret);
}

int
osal_thread_join (uintptr_t tid)
{
    return waitret ( WaitForSingleObject( (HANDLE)tid, INFINITE) );
}


uintptr_t
osal_mutex_create_nonrecursive (void)
{
    return 0;
}

uintptr_t
osal_mutex_create (void)
{
    LPCRITICAL_SECTION mtx = NULL;
    InitializeCriticalSection (mtx);
    return (uintptr_t)mtx ;
}

void
osal_mutex_free (uintptr_t _mtx)
{
    DeleteCriticalSection ( (LPCRITICAL_SECTION)_mtx );
}

int
osal_mutex_lock (uintptr_t _mtx)
{
    EnterCriticalSection ( (LPCRITICAL_SECTION)_mtx );
    return OSAL_RETURN_SUCCESS;
}

int
osal_mutex_unlock (uintptr_t _mtx)
{
    LeaveCriticalSection ( (LPCRITICAL_SECTION)_mtx );
    return OSAL_RETURN_SUCCESS;
}


uintptr_t
osal_cond_create (void)
{
    osal_cmutex_t* cmtx = osal_malloc (sizeof(osal_cmutex_t));
    if (cmtx) {
        cmtx->mutex = CreateMutex (NULL, FALSE, NULL);
        if (cmtx->mutex == NULL) {
            osal_free_null(cmtx);
            goto exit;
        };
        cmtx->event = CreateEvent (NULL, FALSE, FALSE, NULL);
        if (cmtx->event == NULL) {
            (void) CloseHandle (cmtx->mutex);
            osal_free_null(cmtx);
        }
    }
    exit: return (uintptr_t)cmtx;
}

int
osal_cond_lock(uintptr_t _cond)
{
    return waitret ( WaitForSingleObject ( ((osal_cmutex_t*)_cond)->mutex, INFINITE ) );
}

int
osal_cond_unlock (uintptr_t _cond)
{
    return ReleaseMutex (((osal_cmutex_t*)_cond)->mutex) ? OSAL_RETURN_SUCCESS : osal_errno();
}

void
osal_cond_free(uintptr_t _cond)
{
    (void) CloseHandle ( ((osal_cmutex_t*)_cond)->mutex );
    (void) CloseHandle ( ((osal_cmutex_t*)_cond)->event );
    osal_free ( (osal_cmutex_t*)_cond );
}

int
osal_cond_wait (uintptr_t _cond)
{
    DWORD code =
        SignalObjectAndWait( ((osal_cmutex_t*)_cond)->mutex,
                             ((osal_cmutex_t*)_cond)->event,
                             INFINITE, FALSE);
    if (code == WAIT_OBJECT_0)
        code = WaitForSingleObject( ((osal_cmutex_t*)_cond)->mutex, INFINITE);
    return code;
}

int
osal_cond_signal (uintptr_t _cond)
{
    return SetEvent ( ((osal_cmutex_t*)_cond)->event ) ?
                      OSAL_RETURN_SUCCESS : osal_errno();
}

int
osal_cond_broadcast (uintptr_t _cond)
{
    return PulseEvent ( ((osal_cmutex_t*)_cond)->event ) ?
                        OSAL_RETURN_SUCCESS : osal_errno();
}

uintptr_t
osal_rwlock_create (void)
{
    osal_rwmutex_t* _lock = osal_zmalloc (sizeof(osal_cmutex_t));
    if (_lock) {
        _lock->lock_method = SRWLOCK_METHOD_NONE;
        InitializeSRWLock (_lock->handle);
    }
    return (uintptr_t)_lock;
}

void
osal_rwlock_free (uintptr_t _rwl)
{
    osal_free ((osal_rwmutex_t*)_rwl);
}

int
osal_rwlock_rlock (uintptr_t _rwl)
{
    ((osal_rwmutex_t*)_rwl)->lock_method = SRWLOCK_METHOD_SHARED;
    AcquireSRWLockShared ( ((osal_rwmutex_t*)_rwl)->handle );
    return OSAL_RETURN_SUCCESS;
}

int
osal_rwlock_wlock (uintptr_t _rwl)
{
    ((osal_rwmutex_t*)_rwl)->lock_method = SRWLOCK_METHOD_EXCLUSIVE;
    AcquireSRWLockExclusive ( ((osal_rwmutex_t*)_rwl)->handle );
    return OSAL_RETURN_SUCCESS;
}

int
osal_rwlock_unlock (uintptr_t _rwl)
{
    switch (((osal_rwmutex_t*)_rwl)->lock_method){
        case SRWLOCK_METHOD_SHARED:
            {
                ReleaseSRWLockShared (((osal_rwmutex_t*)_rwl)->handle);
                break;
            }
        case SRWLOCK_METHOD_EXCLUSIVE:
            {
                ReleaseSRWLockExclusive (((osal_rwmutex_t*)_rwl)->handle);
                break;
            }
        case SRWLOCK_METHOD_NONE:
            {
                break;
            }
    }
    return OSAL_RETURN_SUCCESS;
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
