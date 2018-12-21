#include "osal.h"
#include <string.h>

char*
ucm_strdup (const char* str)
{
    if (!str)
        return NULL;
    size_t bytes = strlen(str)+1;
    char* ret = ucm_malloc (bytes);
    if (ret)
        memcpy(ret, str, bytes);
    return ret;
}
