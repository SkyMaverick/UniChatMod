#pragma once

uintptr_t
osal_dlopen (const char* path);

void
osal_dlclose (uintptr_t lib);

uintptr_t
osal_dlsym (uintptr_t   lib,
            const char* sym);

char*
osal_dlerror (uintptr_t lib);
