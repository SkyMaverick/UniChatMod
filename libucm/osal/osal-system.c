#include "osal-intrnl.h"

int
osal_errno ()
{
#if defined (AL_OS_WINDOWS)
    return GetLastError();
#else
    return errno;
#endif
}
