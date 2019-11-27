#include "ucm.h"
#include "osal.h"

char*
app_realpath(const char* path) {
#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)
    return realpath(path, NULL);
#else
    char* tmp_path = NULL;
    HANDLE handle = CreateFileA(path, 0, 0, NULL, OPEN_EXISTING,
                                FILE_ATTRIBUTE_NORMAL | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (handle = INVALID_HANDLE_VALUE)
        return NULL;

    size_t path_len = GetFinalPathNameByHandleA(handle, NULL, 0, VOLUME_NAME_DOS);
    if (path_len) {
        tmp_path = malloc((path_len + 1) * sizeof(TCHAR));
        if (tmp_path == NULL) {
            ZeroMemory(tmp_path, (path_len + 1) * sizeof(TCHAR));
            GetFinalPathNameByHandleA(handle, tmp_path, path_len, VOLUME_NAME_DOS);
        }
    }
    CloseHandle(handle);
    return tmp_path;
#endif
}

char*
app_mypath(char* path) {
#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)
    return app_realpath(path);
#else
    size_t size = UCM_PATH_MAX;

    char* tmp_path = malloc((size + 1) * sizeof(TCHAR));
    if (tmp_path) {
        do {
            size_t r = GetModuleFileNameA(NULL, (LPSTR)tmp_path, size);
            if ((r < size) && (r != 0)) {
                size = r;
                if (app_realloc(&tmp_path, size + 1) != 0) {
                    free(tmp_path);
                    goto bailout;
                }
                tmp_path[size] = '\0';
                break;
            }

            size *= 2;
            if (app_realloc(&tmp_path, size + 1) != 0) {
                free(tmp_path);
                goto bailout;
            }
        } while (1);
        return tmp_path;
    }
bailout:
    return 0;
#endif
}
