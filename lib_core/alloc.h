#ifndef _UCM_ALLOC_H_
#define _UCM_ALLOC_H_

#include <stdlib.h>
#include <string.h>

static inline void*
ucm_zmalloc (size_t size)
{
    return calloc (1, size);
}

static inline void*
ucm_calloc (size_t nmem, size_t size)
{
    return calloc (nmem, size);
}

static inline void
ucm_free (void* obj)
{
    free (obj);
}

static inline void
ucm_free_null (void** obj)
{
    free (*obj);
    *obj = NULL;
}

static inline void
ucm_zmemory (void* mem, size_t size)
{
    if (size) {
        if ( size > 4096 ) {
            memset (mem, 0, size);
        } else {
            do {
                size--;
                ((char*)mem)[size] = 0;
            } while (size);
        }
    }
}

#endif
