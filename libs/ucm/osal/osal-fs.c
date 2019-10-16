#include "osal-intrnl.h"

int
osal_fs_fcreate(const char* path)
{
#if defined(UCM_OS_WINDOWS)
    HANDLE hFile =
      CreateFileA((LPCSTR)path, GENERIC_WRITE | GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                  NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        return 1;
    CloseHandle(hFile);
#else
    int hFile = open(path, O_CREAT | O_WRONLY | O_TRUNC, 0664);
    if (!hFile)
        return 1;
    close(hFile);
#endif
    return 0;
}
