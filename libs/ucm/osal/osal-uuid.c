#include "osal-intrnl.h"

#ifdef UCM_OS_WINDOWS

void
osal_uuid_create(ucm_uuid_t uuid) {
    UuidCreate((UUID*)uuid);
}
int
osal_uuid_parse(const char* in, ucm_uuid_t uu) {
    return (UuidFromString((RPC_CSTR)in, (UUID*)uu) == RPC_S_OK) ? 0 : -1;
}
char*
osal_uuid_unparse(const ucm_uuid_t uu) {
    RPC_CSTR out = NULL;

    if (__likely(UuidToString((UUID*)uu, &out)) == RPC_S_OK)
        return (char*)out;

    return NULL;
}
char*
osal_uuid_unparse_lower(const ucm_uuid_t uu) {
    return CharLowerA(osal_uuid_unparse(uu));
}
char*
osal_uuid_unparse_upper(const ucm_uuid_t uu) {
    return CharUpperA(osal_uuid_unparse(uu));
}

int
osal_uuid_is_null(const ucm_uuid_t uu) {
    RPC_STATUS status;
    int ret = UuidIsNil((UUID*)uu, &status);

    return (__likely(status == RPC_S_OK)) ? ret : 0;
}

int
osal_uuid_compare(const ucm_uuid_t uu1, const ucm_uuid_t uu2) {
    RPC_STATUS status;
    int ret = UuidCompare((UUID*)uu1, (UUID*)uu2, &status);

    return (__likely(status == RPC_S_OK)) ? ret : 0;
}
#else  /* UCM_OS_WINDOWS */

void
osal_uuid_create(ucm_uuid_t uuid) {
    uuid_generate(uuid);
}
int
osal_uuid_parse(const char* in, ucm_uuid_t uu) {
    return uuid_parse(in, uu);
}
char*
osal_uuid_unparse(const ucm_uuid_t uu) {
    char* out = osal_zmalloc(sizeof(UUID_STR_SIZE));
    if (__ulikely(!out))
        return NULL;

    uuid_unparse(uu, out);
    return out;
}
char*
osal_uuid_unparse_lower(const ucm_uuid_t uu) {
    char* out = osal_zmalloc(sizeof(UUID_STR_SIZE));
    if (__ulikely(!out))
        return NULL;

    uuid_unparse_lower(uu, out);
    return out;
}
char*
osal_uuid_unparse_upper(const ucm_uuid_t uu) {
    char* out = osal_zmalloc(sizeof(UUID_STR_SIZE));
    if (__ulikely(!out))
        return NULL;

    uuid_unparse_upper(uu, out);
    return out;
}

int
osal_uuid_is_null(const ucm_uuid_t uu) {
    return uuid_is_null(uu);
}

int
osal_uuid_compare(const ucm_uuid_t uu1, const ucm_uuid_t uu2) {
    return uuid_compare(uu1, uu2);
}
#endif /* UCM_OS_WINDOWS */
