#include "osal-intrnl.h"

uintptr_t
osal_diropen_sync (const char* path)
{
    uv_fs_t* dir = osal_zmalloc (sizeof(uv_fs_t));
    if ( AL_LIKELY(dir) ) {
        if ( AL_UNLIKELY(uv_fs_scandir(osal_uv_loop, dir, path, O_RDONLY, NULL)) < 0)
        {
            osal_free_null (dir);
        }
    }
    return (uintptr_t)dir;
}

int
osal_dirnext_sync ( char**    name,
                    uintptr_t iterator)
{
    osal_free_null (*name);

    uv_dirent_t dent;
    if ( AL_UNLIKELY(UV_EOF == uv_fs_scandir_next ((uv_fs_t*)iterator, &dent)) ) {
        return 0;
    }
    *name = osal_strdup (dent.name);
    return dent.type;
}

void
osal_dirclose_sync (uintptr_t iterator)
{
    uv_fs_t* iter = (uv_fs_t*)iterator;
    uv_fs_req_cleanup (iter);
    osal_free (iter);
}

int
osal_access_sync (const char* path,
                  int mode)
{
    uv_fs_t req;
    return uv_fs_access (osal_uv_loop, &req, path, mode, NULL);
}
