#include "osal-intrnl.h"

#include <stdlib.h>
#include <string.h>

void*
osal_malloc(size_t size) {
    return malloc(size);
}

void*
osal_calloc(size_t nmem, size_t size) {
    return (void*)calloc(nmem, size);
}

void*
osal_zmalloc(size_t size) {
    return osal_calloc(1, size);
}

void*
osal_realloc(void* mem, size_t size) {
    return realloc(mem, size);
}

void
osal_free(void* mem) {
    if (mem)
        free(mem);
}
void
osal_zmemory(void* mem, size_t size) {
    if (size) {
        if (__ulikely(size > 4096)) {
            memset(mem, 0, size);
        } else {
            do {
                size--;
                ((char*)mem)[size] = 0;
            } while (size);
        }
    }
}

int
osal_realloc2(void** mem, size_t size) {
    void* old_mem = *mem;
    *mem = osal_realloc(*mem, size);
    if (__likely(*mem != old_mem)) {
        if (__ulikely(*mem == NULL)) {
            *mem = old_mem;
            return 1;
        }
    }
    return 0;
}

void*
osal_memdup(void* mem, size_t size) {
    void* buffer = osal_zmalloc(size);
    if (__likely(buffer))
        memcpy(buffer, mem, size);
    return buffer;
}

char*
osal_strdup(const char* str) {
    size_t len = strlen(str);
    char* buffer = osal_zmalloc(len + 1);
    if (__likely(buffer)) {
        memcpy(buffer, str, len);
    }
    return buffer;
}

char*
osal_strndup(const char* str, size_t num) {
    size_t len = strlen(str);
    if (len < num)
        len = num;
    char* buffer = osal_zmalloc(len + 1);
    if (__likely(buffer)) {
        memcpy(buffer, str, len);
    }
    return buffer;
}
