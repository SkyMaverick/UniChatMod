#ifndef _DB_MDBX_HEADER_
#define _DB_MDBX_HEADER_

#include <inttypes.h>
#include <wchar.h>

#include "ucm.h"
#include "mdbx.h"

#ifdef __cplusplus
    extern "C" {
#endif

extern const ucm_functions_t* app;
extern const ucm_plugdb_t* pldb;

#define DBSYS_VERSION_MAJOR 0
#define DBSYS_VERSION_MINOR 1

#define DBSYS_VERSION   DBSYS_VERSION_MAJOR##'.'##DBSYS_VERSION_MINOR
#define DBSYS_HEADER_SIGNATURE 0x4DBAC0DE

#define trace_dbg(fmt, ...) {app->log ( (ucm_plugin_t*)(&pldb), UCM_TYPE_LOG_DEBUG, fmt, __VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log ( (ucm_plugin_t*)(&pldb), UCM_TYPE_LOG_INFO,  fmt, __VA_ARGS__);}
#define trace_err(fmt, ...) {app->log ( (ucm_plugin_t*)(&pldb), UCM_TYPE_LOG_ERROR, fmt, __VA_ARGS__);}

#define ucm_free_null(X)    \
    do {                    \
        app->free(X);       \
        X = NULL;           \
    } while (0)

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
    uint64_t    version;
} db_header_t;

typedef struct {
    struct {
        MDBX_env*       env;
        MDBX_txn*       txn_ro;

        MDBX_dbi        dbi_global;
        MDBX_cursor*    cur_global;

        MDBX_dbi        dbi_events;
        MDBX_cursor*    cur_events;

        MDBX_dbi        dbi_contacts;
        MDBX_cursor*    cur_contacts;

        MDBX_dbi        dbi_settings;
        MDBX_cursor*    cur_settings;

        MDBX_dbi        dbi_logs;
        MDBX_cursor*    cur_logs;
    } mdbx;

    struct {
        int dummy;
        // TODO events variables
    } events;

    struct {
        int dummy;
        // TODO logs variables
    } logs;

    struct {
        int dummy;
        // TODO contacts variables
    } contacts;

    db_header_t header;

    uintptr_t   mtx;
    uint32_t    flags;
    
    char        faPath [1];
} db_object_t;

#define DBTABLE_NAME_GLOBAL     "global"
#define DBTABLE_NAME_SETTINGS   "settings"
#define DBTABLE_NAME_EVENTS     "events"
#define DBTABLE_NAME_CONTACTS   "contacts"
#define DBTABLE_NAME_LOGS       "logs"

extern const ucm_functions_t* app;
extern db_object_t* UCM_DB;

static inline MDBX_txn*
StartTxn (db_object_t* db)
{
    MDBX_txn *res = 0;
    int rc = mdbx_txn_begin (
                db->mdbx.env,
                NULL,
                (db->flags & UCM_FLAG_DB_READONLY) ? MDBX_RDONLY : 0,
                &res );
    // TODO exception
    UNUSED (rc);
    return res;
}

static inline uint64_t
MakeLong    (uint32_t x,
             uint32_t y)
{
    return (((uint64_t)(x)) << 32) | (y);
}

void
_assert_func (const MDBX_env *env,
              const char     *msg,
              const char     *function,
              unsigned       line);

#ifdef __cplusplus
    }
#endif

#endif
