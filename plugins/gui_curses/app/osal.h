#pragma once

char*
app_realpath (const char*  path);

inline int
app_realloc (void** mem,
             size_t size)
{
    void* old_mem = *mem;
    *mem = realloc (*mem, size);
    if ( *mem != old_mem ) {
        if ( *mem == NULL ) {
            *mem = old_mem;
            return 1;
        }
    }
    return 0;
}

char*
app_mypath (char* path);
