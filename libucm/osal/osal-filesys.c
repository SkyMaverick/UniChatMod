#include <plibsys.h>

typedef struct {
    PDir*   dir;
} dir_handler_t;

uintptr_t 
osal_idir_create (const char* path)
{   
    if (p_dir_is_exists (path)) {
        dir_handler_t* iterator = osal_zmalloc (sizeof(dir_handler_t));
        if (iterator) {
            iterator->dir = p_dir_new (path, NULL);
            return iterator;
        }
    }
    return 0;
}

int 
osal_idir_next (char**    name,
                uintptr_t iterator)
{
    /* WARNING! plibsys internal check NULL-value
       if remake other lib - check this code */
    osal_free (*name);
    dir_handler_t* iter = (dir_handler_t*)iterator;
    
    PDirEntry* entry = p_dir_get_next_entry (iter->dir, NULL);
    if (entry) {
        *name = osal_strdup (entry->name);
        int ret = entry->type;

        p_dir_entry_free (entry);
        return ret;
    }
    return 0;
}

bool
osal_idir_rollback (uintptr_t iterator)
{
    return p_dir_rewind ( ((dir_handler_t*)iterator)->dir, NULL );
}

void
osal_idir_release (uintptr_t iterator)
{
    dir_handler_t* iter = (dir_handler_t*) iterator;
    p_dir_free (iter->dir); 
    osal_free (iter);
}
