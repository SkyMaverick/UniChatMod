#pragma once

#ifdef UCM_OS_WINDOWS
    #pragma comment(lib, "rpcrt4.lib")
#else
    #ifdef ENABLE_CUSTOM_LIBS
        #include "uuid.h"
    #else
        #include <uuid/uuid.h>
    #endif
#endif

#define UUID_STR_SIZE 37

void
osal_uuid_create(ucm_uuid_t uuid);
int
osal_uuid_parse(const char* in, ucm_uuid_t uu);
char*
osal_uuid_unparse(const ucm_uuid_t uu);
char*
osal_uuid_unparse_lower(const ucm_uuid_t uu);
char*
osal_uuid_unparse_upper(const ucm_uuid_t uu);
int
osal_uuid_is_null(const ucm_uuid_t uu);
int
osal_uuid_compare(const ucm_uuid_t uu1, const ucm_uuid_t uu2);
