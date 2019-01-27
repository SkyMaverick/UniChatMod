#include <plibsys.h>

inline uintptr_t
osal_dlopen (const char* path)
{
    return (uintptr_t) p_library_loader_new (path);
}

inline void
osal_dlclose (uintptr_t lib)
{
    p_library_loader_free ( (PLibraryLoader*)lib );
}

inline uintptr_t
osal_dlsym (uintptr_t   lib,
            const char* sym)
{
    return (uintptr_t) p_library_loader_get_symbol ((PLibraryLoader*)lib, sym);
}

inline char*
osal_dlerror (uintptr_t lib)
{
    return p_library_loader_get_last_error( (PLibraryLoader*)lib );
}
