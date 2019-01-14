#include "osal.h"
/* ======================================================================
        DIRECTORY WALK FUNCTIONS
   ====================================================================== */

typedef struct {
    DIR* handle;
    char path [1];
} posix_dir_t;

static int
get_fso_type (posix_dir_t* dir,
              const char*  fso)
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
                case S_IFREG: return OSAL_DTYPE_FILE;
                case S_IFDIR: return OSAL_DTYPE_DIRECTORY;

                default: return OSAL_DTYPE_UNKNOW;
            }
        }
    }
    return OSAL_DTYPE_UNKNOW;
}

osal_dir_t
osal_diropen (const char*   path,
             osal_dirent_t* list)
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
            
                list->name      = tmp->d_name;
                list->type      = get_fso_type (pdir, list->name);
                list->__sysdata = tmp;
            }
            return (osal_dir_t) pdir;
        }
    }
    osal_zmemory (list, sizeof(osal_dirent_t));
    return 0;
}

int
osal_dirnext (osal_dir_t    dir,
             osal_dirent_t* list)
{
    osal_zmemory(list, sizeof(osal_dirent_t));

    struct dirent* tmp = readdir (((posix_dir_t*)dir)->handle);
    if (tmp == NULL)
        return 0;
    
    list->name      = tmp->d_name;
    list->type      = get_fso_type ((posix_dir_t*)dir, list->name);
    list->__sysdata = tmp;

    return 1;
}

void
osal_dirclose (osal_dir_t dir)
{
    closedir ( ((posix_dir_t*)dir)->handle );
    osal_free ( (posix_dir_t*) dir);
}

/* ======================================================================
        FILE ACCESS CHECKING FUNCTIONS
   ====================================================================== */
