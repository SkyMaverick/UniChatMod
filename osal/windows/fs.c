#include "osal.h"

/* ======================================================================
         FILE SYSTEM WALK FUNCTIONS
   ====================================================================== */

static inline int
get_fso_type (WIN32_FIND_DATA* data)
{
    if (data->dwFileAttributes == FILE_ATTRIBUTE_DIRECTORY)
        return OSAL_DTYPE_DIRECTORY;
    if (data->dwFileAttributes == FILE_ATTRIBUTE_NORMAL)
        return OSAL_DTYPE_FILE;
    return OSAL_DTYPE_UNKNOW;
}

osal_dir_t
osal_diropen (const char*    path,
              osal_dirent_t* list)
{
    HANDLE ret = FindFirstFile(path, &(list->__sysdata));
    if (ret != INVALID_HANDLE_VALUE) {
        list->name = list->__sysdata.cFileName;
        list->type = get_fso_type (&(list->__sysdata));
        return (osal_dir_t) ret;
    }
    osal_zmemory (list, sizeof(osal_dirent_t));
    return 0;
}

int
osal_dirnext (osal_dir_t     dir,
              osal_dirent_t* list)
{
    osal_zmemory (list, sizeof(osal_dirent_t));
    int ret = FindNextFile ((HANDLE)dir, &(list->__sysdata));
    if (ret) {
        list->name = list->__sysdata.cFileName;
        list->type = get_fso_type (&(list->__sysdata));
    }
    return ret;
}

void
osal_dirclose (osal_dir_t dir)
{
    FindClose ( (HANDLE)dir );
}

/* ======================================================================
         FILE ACCESS FUNCTIONS
   ====================================================================== */
