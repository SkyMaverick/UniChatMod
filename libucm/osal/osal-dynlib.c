#include "osal.h"

uintptr_t
osal_dlopen (const char* path)
{
    uv_lib_t* lib = osal_malloc(sizeof(uv_lib_t));
    if ( __likely(lib) ) {
        if ( __likely (uv_dlopen(path, lib) >= 0) ) {
            return (uintptr_t)lib;
        } else {
            osal_free (lib);
        }
    }
    return 0;
}

void
osal_dlclose (uintptr_t lib)
{
    uv_dlclose ( (uv_lib_t*)lib );
    osal_free ( (uv_lib_t*)lib );
}

uintptr_t
osal_dlsym (uintptr_t   lib,
            const char* sym)
{
    void* func = NULL;
    return (uv_dlsym ((uv_lib_t*)lib, sym, &func) < 0) ?
            0 : (uintptr_t)(func);
}

const char*
osal_dlerror (uintptr_t lib)
{
    return uv_dlerror( (uv_lib_t*)lib );
}
