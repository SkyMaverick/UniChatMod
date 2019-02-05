#pragma once

typedef enum {
    OSAL_DIRENT_UNKNOWN,
    OSAL_DIRENT_FILE,
    OSAL_DIRENT_DIR,
    OSAL_DIRENT_LINK,
    OSAL_DIRENT_FIFO,
    OSAL_DIRENT_SOCKET,
    OSAL_DIRENT_CHAR,
    OSAL_DIRENT_BLOCK
} osal_dirent_type_t;

uintptr_t 
osal_diropen_sync (const char* path);

int 
osal_dirnext_sync ( char**    name,
                    uintptr_t iterator);
void
osal_dirclose_sync (uintptr_t iterator);

int
osal_access_sync (const char* path,
                  int mode);
