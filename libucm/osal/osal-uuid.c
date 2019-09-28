#include "osal-intrnl.h"

#ifdef UCM_OS_WINDOWS
void
osal_uuid_create(ucm_uuid_t uuid)
{
    UUID out;
    UuidCreate(&out);

    memcpy(&uuid, &out, sizeof(ucm_uuid_t));
}

int
osal_uuid_parse(const char* in, ucm_uuid_t uu)
{
    UUID tmp;
    if (UuidFromString((RPC_CSTR)in, &tmp) == RPC_S_OK) {
        memcpy(&uu, &tmp, sizeof(ucm_uuid_t));
        return 0;
    }
    return -1;
}
char*
osal_uuid_unparse(const ucm_uuid_t uu)
{
    UUID tmp;
    RPC_CSTR out = NULL;

    memcpy(&tmp, &uu, sizeof(ucm_uuid_t));

    if (UuidToString(&tmp, &out) == RPC_S_OK)
        return (char*)out;

    return NULL;
}
char*
osal_uuid_unparse_lower(const ucm_uuid_t uu)
{
    return CharLowerA(osal_uuid_unparse(uu));
}
char*
osal_uuid_unparse_upper(const ucm_uuid_t uu)
{
    return CharUpperA(osal_uuid_unparse(uu));
}

#else /* UCM_OS_WINDOWS */

void
osal_uuid_create(ucm_uuid_t uuid)
{
    uuid_generate(uuid);
}
int
osal_uuid_parse(const char* in, ucm_uuid_t uu)
{
    return uuid_parse(in, uu);
}
char*
osal_uuid_unparse(const ucm_uuid_t uu)
{
    char* out = osal_zmalloc(sizeof(UUID_STR_SIZE));
    if (!out)
        return NULL;

    uuid_unparse(uu, out);
    return out;
}
char*
osal_uuid_unparse_lower(const ucm_uuid_t uu)
{
    char* out = osal_zmalloc(sizeof(UUID_STR_SIZE));
    if (!out)
        return NULL;

    uuid_unparse_lower(uu, out);
    return out;
}
char*
osal_uuid_unparse_upper(const ucm_uuid_t uu)
{
    char* out = osal_zmalloc(sizeof(UUID_STR_SIZE));
    if (!out)
        return NULL;

    uuid_unparse_upper(uu, out);
    return out;
}

#endif /* UCM_OS_WINDOWS */
