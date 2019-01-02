#include "osal.h"

/*POSIX thread api wrapper
  based on deadbeef/threading_pthread by Alexey Yakovenko */

#include <pthread.h>

/* ======================================================================
        CUSTOM THREADING FUNCTIONS
   ====================================================================== */

typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t  cond;
} osal_cmutex_t;

uintptr_t osal_thread_create (
        THREAD_RESULT ( THREAD_CALL *func)(void* ctx),
        void* ctx)
{
    pthread_t tid;
    pthread_attr_t attr;
    
    pthread_attr_init (&attr);
    pthread_create(&tid, &attr, func, ctx);
    pthread_attr_destroy(&attr);
    
    return (uintptr_t)tid;
}


inline int
osal_thread_detach (uintptr_t tid)
{
    return pthread_detach((pthread_t)tid) ? 1 : 0;
}

inline void
osal_thread_exit (void* ret)
{
    pthread_exit(ret);
}

int
osal_thread_join (uintptr_t tid)
{
    void* ret;
    return pthread_join((pthread_t)tid,&ret) ? 1 : 0;
}


uintptr_t
osal_mutex_create_nonrecursive (void)
{
    pthread_mutex_t* mtx= osal_malloc(sizeof(pthread_mutex_t));
    if (mtx) {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_TIMED_NP);
#endif
        if ( pthread_mutex_init(mtx,&attr) ) {
            osal_free_null (mtx);
        }
        pthread_mutexattr_destroy (&attr);
    }
    return (uintptr_t)mtx;
}

uintptr_t
osal_mutex_create (void)
{
    pthread_mutex_t* mtx= osal_malloc(sizeof(pthread_mutex_t));
    if (mtx) {
        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE_NP);
#endif
        if ( pthread_mutex_init(mtx,&attr) ) {
            osal_free_null (mtx);
        }
        pthread_mutexattr_destroy (&attr);
    }
    return (uintptr_t)mtx;
}

void
osal_mutex_free (uintptr_t _mtx)
{
    if (_mtx) {
        pthread_mutex_t* mtx = (pthread_mutex_t*)_mtx;
        pthread_mutex_destroy(mtx);
        osal_free(mtx);
    }
}

inline int
osal_mutex_lock(uintptr_t _mtx)
{
    return pthread_mutex_lock ( (pthread_mutex_t*)_mtx );
}

inline int
osal_mutex_unlock(uintptr_t _mtx)
{
    return pthread_mutex_unlock ( (pthread_mutex_t*)_mtx );
}

uintptr_t
osal_cond_create (void)
{
    osal_cmutex_t *cmtx = osal_malloc(sizeof(osal_cmutex_t));
    if (cmtx) {
        if ( pthread_cond_init(&(cmtx->cond), NULL) ) {
            osal_free_null (cmtx);
            goto exit;
        };

        pthread_mutexattr_t attr;
        pthread_mutexattr_init(&attr);
#if defined (__GLIBC__)
        pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE_NP);
#endif
        if ( pthread_mutex_init (&(cmtx->mtx),&attr) ) {
            osal_free_null (cmtx);
        }
        pthread_mutexattr_destroy (&attr);
    }
    exit:return (uintptr_t)cmtx;
}

inline int
osal_cond_lock (uintptr_t _cond)
{
    return osal_mutex_lock ( (uintptr_t)(&(((osal_cmutex_t*)_cond)->mtx)) );
}

inline int
osal_cond_unlock (uintptr_t _cond)
{
    return osal_mutex_unlock ( (uintptr_t)(&(((osal_cmutex_t*)_cond)->mtx)) );
}

void
osal_cond_free(uintptr_t _cond)
{
    if (_cond) {
        osal_cmutex_t* cmtx = (osal_cmutex_t*)_cond;
        pthread_cond_destroy ( &(cmtx->cond) );
        pthread_mutex_destroy ( &(cmtx->mtx) );
        osal_free (cmtx);
    }
}

int
osal_cond_wait (uintptr_t _cond)
{
    osal_cmutex_t* cmtx = (osal_cmutex_t*) _cond;
    int fail = osal_mutex_lock ( (uintptr_t)(&(cmtx->mtx)) );
    
    return fail ? fail : pthread_cond_wait (&(cmtx->cond),
                                            &(cmtx->mtx));
}

inline int
osal_cond_signal (uintptr_t _cond)
{
    return pthread_cond_signal (&(((osal_cmutex_t*) _cond)->cond));
}

inline int
osal_cond_broadcast (uintptr_t _cond)
{
    return pthread_cond_broadcast ( &( ((osal_cmutex_t*)_cond)->cond ) );
}

uintptr_t
osal_rwlock_create (void)
{
    pthread_rwlock_t* rwl = osal_malloc(sizeof(pthread_rwlock_t));
    if (rwl) {
        pthread_rwlockattr_t attr;
        pthread_rwlockattr_init(&attr);
#if defined (__GLIBC__)
        pthread_rwlockattr_setkind_np(&attr,PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
#endif
        if ( pthread_rwlock_init(rwl,&attr) ) {
            osal_free_null (rwl);
        }
        pthread_rwlockattr_destroy(&attr);
    }
    return (uintptr_t)rwl;
}

void
osal_rwlock_free (uintptr_t _rwl)
{
    if (_rwl) {
        pthread_rwlock_t* rwl = (pthread_rwlock_t*)_rwl;
        pthread_rwlock_destroy(rwl);
        osal_free(rwl);
    }
}

inline int
osal_rwlock_rlock (uintptr_t _rwl)
{
    return pthread_rwlock_rdlock ((pthread_rwlock_t*)_rwl);
}

inline int
osal_rwlock_wlock (uintptr_t _rwl)
{
    return pthread_rwlock_wrlock ((pthread_rwlock_t*)_rwl);
}

inline int
osal_rwlock_unlock(uintptr_t _rwl)
{
    return pthread_rwlock_unlock ((pthread_rwlock_t*)_rwl);
}

/* ======================================================================
        CUSTOM FUNCTIONS
   ====================================================================== */
static int
get_fso_type (posix_dir_t*  dir,
              const char*   fso)
{
    struct stat sb;
    uint32_t buffer_lenght = strlen (dir->path)  + strlen (fso) + 2;
    char* path_fabs = osal_zmalloc (buffer_lenght);
    if (path_fabs) {
        
        snprintf (path_fabs, buffer_lenght, "%s/%s", dir->path, fso);
        int st_ret = lstat(path_fabs, &sb);
        osal_free (path_fabs);

        if ( st_ret != -1) {
            switch (sb.st_mode & S_IFMT) {
                // TODO Make new type is need
                case S_IFREG: return FO_TYPE_FILE;
                case S_IFDIR: return FO_TYPE_DIRECTORY;

                default: return FO_TYPE_UNKNOW;
            }
        }
    }
    return FO_TYPE_UNKNOW;
}

osal_dir_t
osal_diropen (const char*       path,
             osal_fsobject_t*   list)
{
    if (list == NULL)
        return 0;
    DIR* folder = opendir (path);
    if (folder != NULL) {
        struct dirent* tmp = readdir (folder);
        if (tmp) {
            posix_dir_t* pdir = osal_zmalloc ((strlen(path) + 1) * sizeof(char)
                                             + sizeof (posix_dir_t) );
            if (pdir) {
                pdir->handle = folder;
                memcpy (pdir->path, path, strlen (path));
            
                list->name   = tmp->d_name;
                list->handle = (uintptr_t) tmp;
                list->type   = get_fso_type (pdir, list->name);
            }
            return (osal_dir_t) pdir;
        }
    }
    osal_zmemory (list, sizeof(osal_fsobject_t));
    return 0;
}

int
osal_dirnext (osal_dir_t         dir,
             osal_fsobject_t*   list)
{
    osal_zmemory(list, sizeof(osal_fsobject_t));

    struct dirent* tmp = readdir (((posix_dir_t*)dir)->handle);
    if (tmp == NULL)
        return 0;
    
    list->name = tmp->d_name;
    list->handle = (uintptr_t) tmp;
    list->type   = get_fso_type ((posix_dir_t*)dir, list->name);

    return 1;
}

void
osal_dirclose (osal_dir_t dir)
{
    closedir ( ((posix_dir_t*)dir)->handle );
    osal_free ( (posix_dir_t*) dir);
}
