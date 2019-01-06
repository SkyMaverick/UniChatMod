#include "osal.h"

#if defined(_WIN32) || defined(_WIN64)
    #include "windows/fs.c"
    #include "windows/thread.c"
#else
    #include "linux/fs.c"
    #include "linux/thread.c"
#endif
#include "common.c"

