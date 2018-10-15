#ifndef _UCM_ALLOC_H_
#define _UCM_ALLOC_H_

#include <stdlib.h>
#include <string.h>

static inline void*
ucm_kmalloc (size_t size)
{
    return malloc(size);
}

static inline void*
ucm_kzmalloc (size_t size)
{
    return calloc (1, size);
}

static inline void*
ucm_kcalloc (size_t nmem, 
            size_t size)
{
    return calloc (nmem, size);
}

static inline void
ucm_kfree (void* obj)
{
    free (obj);
}

static inline void
ucm_kzmemory (void*  mem,
             size_t size)
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

static inline int
ucm_krealloc (void**  mem,
             size_t size)
{
    void* nblk = realloc(*mem, size);
    if (nblk) {
        *mem = nblk;
        return 0;
    }
    return 1;    
}

#endif
