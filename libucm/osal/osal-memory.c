#include <string.h>
#include "osal.h"

inline void*
osal_malloc (size_t size)
{
    return (void*) p_malloc(size);
}
inline void*
osal_zmalloc (size_t size)
{
    return (void*) p_malloc0(size);
}
inline void*
osal_calloc (size_t nmem,
             size_t size)
{
    return (void*) osal_zmalloc( nmem * size );
}
inline void
osal_free (void* mem)
{
    p_free (mem);
}
void
osal_zmemory (void* mem,
              size_t size)
{
    if (size) {
        if ( P_UNLIKELY(size > 4096) ) {
            memset (mem, 0, size);
        } else {
            do {
                size--;
                ((char*)mem)[size] = 0;
            } while (size);
        }
    }
}
int
osal_realloc (void** mem, size_t size)
{
    void* old_mem = *mem;
    *mem = p_realloc (*mem, size);
    if ( P_LIKELY(*mem != old_mem) ) {
        if ( P_UNLIKELY(*mem != NULL) ) {
            osal_free (old_mem);
        } else {
            *mem = old_mem;
        }
        return 1;
    }
    return 0;
}

inline char*
osal_strdup (const char* str)
{
    return p_strdup (str);
}
