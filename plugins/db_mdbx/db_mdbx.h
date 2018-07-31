#ifndef _DB_MDBX_HEADER_
#define _DB_MDBX_HEADER_

#include <inttypes.h>
#include "ucm.h"
#include "libmdbx/mdbx.h"

#ifdef __cplusplus
    extern "C" {
#endif

enum {
    DB_VALUE_BYTE   ,
    DB_VALUE_WORD   ,
    DB_VALUE_DWORD  ,
    DB_VALUE_QWORD  ,
    DB_VALUE_STRING ,
    DB_VALUE_BLOB
};

typedef struct {
    uint8_t         type;
    union {
        uint8_t     byte;
        uint16_t    word;
        uint32_t    d_word;
        uint64_t    q_word;
        char        string[1];
        struct {
            size_t  lenght;
            uint8_t data[1];
        } blob;
    };
} db_value_t;

typedef struct {
    uint32_t    signature;
    uint32_t    version;
} db_header_t;

enum {
    DB_FLAG_READONLY = 1 << 0
};

typedef struct {
    struct {
        MDBX_env*       env;
        MDBX_txn*       txn;

        MDBX_dbi        dbi_global;
        MDBX_cursor*    cur_global;

        MDBX_dbi        dbi_events;
        MDBX_cursor*    cur_events;

        MDBX_dbi        dbi_contacts;
        MDBX_cursor*    cur_contacts;

        MDBX_dbi        dbi_plugins;
        MDBX_cursor*    cur_plugins;
    } mdbx;

    db_header_t header;

    uintptr_t   mtx;
    char        file_apath [PATH_MAX];
    uint32_t    flags;
} db_object_t;

extern const ucm_functions_t* app;
extern db_object_t* UCM_DB;

void
_assert_func (const MDBX_env *env, 
              const char     *msg,
              const char     *function,
              unsigned       line);

#ifdef __cplusplus
    }
#endif

#endif
