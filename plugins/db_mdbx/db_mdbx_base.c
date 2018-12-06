#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#include "ucm.h"
#include "api.h"
#include "threading.h"
#include "db_mdbx.h"
#include "db_mdbx_base.h"

#include "mdbx.h"

const ucm_functions_t* app;
#define DBMDBX db->mdbx

static UCM_RET
__db_map (db_object_t* db)
{
    mdbx_env_create         (&(DBMDBX.env));
    mdbx_env_set_maxdbs       (DBMDBX.env, 10);
    mdbx_env_set_maxreaders   (DBMDBX.env, 244);
    mdbx_env_set_userctx      (DBMDBX.env, UCM_DB);
    mdbx_env_set_assert       (DBMDBX.env, _assert_func);

    int rc = mdbx_env_set_geometry (DBMDBX.env,
                     -1,
              1ul << 20,
            512ul << 20,
              1ul << 20,
            512ul << 10,
                     -1);
    if (rc != MDBX_SUCCESS)
        return UCM_RET_EXCEPTION;

    unsigned int mode =   MDBX_NOSUBDIR
                        | MDBX_MAPASYNC
                        | MDBX_WRITEMAP
                        | MDBX_NOSYNC
                        | MDBX_COALESCE
                        | MDBX_EXCLUSIVE
                        ;
    if (db->flags & UCM_FLAG_DB_READONLY) {
        mode |= MDBX_RDONLY;
    }

    return ( mdbx_env_open (DBMDBX.env, db->faPath, mode, 0664) != MDBX_SUCCESS )
           ? UCM_RET_NOACCESS : UCM_RET_SUCCESS;
}

static inline UCM_RET
__db_intrnl_load (db_object_t* db)
{
    unsigned  flags     = (db->flags & UCM_FLAG_DB_READONLY) ? 0 : MDBX_CREATE;
    MDBX_txn* txn_tmp   = StartTxn(db);
    if (txn_tmp) {

        /* Open all tables */

        if (
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_GLOBAL,   flags | MDBX_INTEGERKEY, &(DBMDBX.dbi_global))                   == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_SETTINGS, flags | MDBX_INTEGERKEY, &(DBMDBX.dbi_settings))                 == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_EVENTS,   flags | MDBX_INTEGERKEY, &(DBMDBX.dbi_events))                   == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_CONTACTS, flags | MDBX_INTEGERKEY, &(DBMDBX.dbi_contacts))                 == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open_ex (txn_tmp, DBTABLE_NAME_LOGS,     flags | MDBX_INTEGERKEY, &(DBMDBX.dbi_logs), settings_cmp, NULL) == MDBX_SUCCESS )
        ) {
            uint32_t kVal = 1;
            MDBX_val key = {&kVal, sizeof(kVal)}, data;

            /* Get database header and valide this.
               If header broken recreate it if check CREATENEW flag and don't check READONLY flag
             */

            if ( mdbx_get(txn_tmp, DBMDBX.dbi_global, &key, &data) == MDBX_SUCCESS ) {
                // TODO
                const db_header_t* hdr = (const db_header_t*) data.iov_base;
                if (hdr) {
                    /* Databse signature valide  */
                    if (hdr->signature != DBSYS_HEADER_SIGNATURE) {
                        trace_err ("%s\n", "Header signature not found. Maybe this not UCM database.");
                        return UCM_RET_INVALID;
                    }
                    /*  Database format version valide */
                    if (hdr->version != MakeLong (DBSYS_VERSION_MAJOR, DBSYS_VERSION_MINOR)) {
                        trace_err ("%s: %d \n", "Bad database major version. Need convert to", DBSYS_VERSION_MAJOR);
                        return UCM_RET_BADVERSION;
                    }
                    db->header = * hdr;

                } else {
                    trace_dbg ("%s\n", "Return NULL header");
                    return UCM_RET_EMPTY;
                }
            /* Create new header with new tables */
            } else {
                if ((db->flags & UCM_FLAG_DB_CREATENEW) &&
                    (!(db->flags & UCM_FLAG_DB_READONLY)) )
                {
                    db->header.signature = DBSYS_HEADER_SIGNATURE;
                    db->header.version   = MakeLong (DBSYS_VERSION_MAJOR, DBSYS_VERSION_MINOR);

                    data.iov_base = (void*)(&(db->header)), data.iov_len = sizeof(db_header_t);
                    if ( mdbx_put(txn_tmp, DBMDBX.dbi_global, &key, &data, 0) == MDBX_SUCCESS ) {
                        db_flush (db);
                    } else {
                        trace_err ("%s: %s\n", "Create database error", mdbx_strerror(errno));
                        return UCM_RET_EXCEPTION;
                    }
                } else {
                    trace_err ("%s : %s\n", "Open database error", mdbx_strerror(errno));
                    return UCM_RET_EXCEPTION;
                }
            }

            mdbx_txn_commit(txn_tmp);
        }
    }

    mdbx_txn_begin (DBMDBX.env, NULL, MDBX_RDONLY, &(DBMDBX.txn_ro));

    mdbx_cursor_open (DBMDBX.txn_ro, DBMDBX.dbi_events  , &(DBMDBX.cur_events));
    mdbx_cursor_open (DBMDBX.txn_ro, DBMDBX.dbi_contacts, &(DBMDBX.cur_contacts));
    mdbx_cursor_open (DBMDBX.txn_ro, DBMDBX.dbi_settings, &(DBMDBX.cur_settings));
    mdbx_cursor_open (DBMDBX.txn_ro, DBMDBX.dbi_logs    , &(DBMDBX.cur_logs));

    mdbx_txn_reset (DBMDBX.txn_ro);

    return UCM_RET_SUCCESS;
}

UCM_RET
db_open (db_object_t* db)
{
    int fhandle;
    if (!db || (db->faPath[0] == '\0'))
        return UCM_RET_INVALID;
    if ( access(db->faPath, F_OK) < 0) {
        if (!(db->flags & UCM_FLAG_DB_READONLY)) {
            if ( ( fhandle = creat(db->faPath, 0664) ) < 0) {
                trace_err ("%s: %s\n", "Database error: ", strerror(errno));
                return UCM_RET_NOACCESS;
            } else {
                db->flags |= UCM_FLAG_DB_CREATENEW;
                close (fhandle);
            }
        } else {
            trace_err ("%s: %s\n", "Database error: ", strerror(errno));
            return UCM_RET_NOOBJECT;
        }
    } else {
        if ( access (db->faPath, (db->flags & UCM_FLAG_DB_READONLY) ? R_OK: R_OK | W_OK) < 0) {
            trace_err ("%s: %s\n", "Database error: ", strerror(errno));
            return UCM_RET_NOACCESS;
        }
    }
    return __db_intrnl_load(db);
}

UCM_RET
db_flush (db_object_t* db)
{
    return UCM_RET_SUCCESS;
}

UCM_RET
db_close ()
{
    return UCM_RET_SUCCESS;
}

UCM_RET
db_check (db_object_t* db)
{
    return UCM_RET_SUCCESS;
}

UCM_RET
db_backup (char* bfName)
{
    return UCM_RET_SUCCESS;
}

#undef DBMDBX
