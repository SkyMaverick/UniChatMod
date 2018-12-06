/*POSIX thread api wrapper*/

#ifndef PTHREADS_H_
#define PTHREADS_H_
#include "stdint.h"

intptr_t thread_create(void(*func)(void* ctx), void* ctx);
int thread_detach(intptr_t tid) ;
void thread_exit(void* ret) ;
int thread_join(intptr_t tid) ;

uintptr_t mutex_create_nonrecursive(void) ;
uintptr_t mutex_create(void) ;
void mutex_free(uintptr_t _mtx) ;
int mutex_lock(uintptr_t _mtx) ;
int mutex_unlock(uintptr_t _mtx) ;

uintptr_t cond_create (void) ;
void cond_free(uintptr_t _cond) ;
int cond_wait(uintptr_t _cond, uintptr_t _mtx) ;
int cond_signal (uintptr_t _cond) ;
int cond_broadcast (uintptr_t _cond) ;

uintptr_t rwlock_create(void);
void rwlock_free(uintptr_t _rwl);
int rwlock_rlock(uintptr_t _rwl);
int rwlock_wlock(uintptr_t _rwl);
int rwlock_unlock(uintptr_t _rwl);
#endif
