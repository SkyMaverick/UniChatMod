#pragma once

uintptr_t
osal_thread_create(void* (*func)(void* ctx), void* ctx);
int
osal_thread_detach(uintptr_t tid);
void*
osal_thread_exit(void);
int
osal_thread_join(uintptr_t tid);
void
osal_thread_cleanup(uintptr_t* tid);
uintptr_t
osal_mutex_create(void);
void
osal_mutex_free(uintptr_t _mtx);
void
osal_mutex_lock(uintptr_t _mtx);
int
osal_mutex_trylock(uintptr_t _mtx);
void
osal_mutex_unlock(uintptr_t _mtx);
uintptr_t
osal_cond_create(void);
void
osal_cond_free(uintptr_t _cond);
void
osal_cond_wait(uintptr_t _cond, uintptr_t _mtx);
void
osal_cond_signal(uintptr_t _cond);
void
osal_cond_broadcast(uintptr_t _cond);

uintptr_t
osal_rwlock_create(void);
void
osal_rwlock_free(uintptr_t _rwl);
int
osal_rwlock_tryrlock(uintptr_t _rwl);
int
osal_rwlock_trywlock(uintptr_t _rwl);
void
osal_rwlock_rlock(uintptr_t _rwl);
void
osal_rwlock_wlock(uintptr_t _rwl);
void
osal_rwlock_unlock(uintptr_t _rwl);
