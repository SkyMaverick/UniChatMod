#include "osal.h"
#include <string.h>

char*
osal_strdup (const char* str)
{
    if (!str)
        return NULL;
    size_t bytes = strlen(str)+1;
    char* ret = osal_malloc (bytes);
    if (ret)
        memcpy(ret, str, bytes);
    return ret;
}
