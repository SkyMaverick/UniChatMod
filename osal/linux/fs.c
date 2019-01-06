/* ======================================================================
        DIRECTORY WALK FUNCTIONS
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
