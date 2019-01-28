#pragma once
uintptr_t 
osal_idir_create (const char* path);

int 
osal_idir_next (char**    name,
                uintptr_t iterator);
bool
osal_idir_rollback (uintptr_t iterator);

void
osal_idir_release (uintptr_t iterator);

bool
osal_dir_exists (const char* path);

bool
osal_file_exists (const char* path);
