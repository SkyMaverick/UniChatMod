#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "ucm.h"
#include "config.h"
#include "gettext.h"

#include "mdbx.h"

#include "db_mdbx.h"
#include "mdbx_core.h"

static int
__log_cmp_helper (const MDBX_val* a,
                  const MDBX_val* b)
{
    // TODO make
    UNUSED(a);
    UNUSED(b);

    return 0;
}

static UCM_RET
__mdbx_map (void)
{
    mdbx_env_create         (&(UniDB->mdbx.env));
    mdbx_env_set_maxdbs       (UniDB->mdbx.env, 10);
    mdbx_env_set_maxreaders   (UniDB->mdbx.env, 244);
    mdbx_env_set_userctx      (UniDB->mdbx.env, UniDB);
    mdbx_env_set_assert       (UniDB->mdbx.env, _assert_func);

    int rc = mdbx_env_set_geometry (UniDB->mdbx.env,
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
    if (UniDB->flags | UCM_FLAG_DB_READONLY) {
        mode |= MDBX_RDONLY;
    }

    return ( mdbx_env_open (UniDB->mdbx.env,
                            app->app.get_store_path(),
                            mode,
                            0664) != MDBX_SUCCESS ) ? UCM_RET_NOACCESS :
                                                      UCM_RET_SUCCESS;
}

static inline UCM_RET
__mdbx_load (void)
{
    unsigned  flags     = (UniDB->flags & UCM_FLAG_DB_READONLY) ? 0 : MDBX_CREATE;
    MDBX_txn* txn_tmp   = StartTxn(UniDB);
    if (txn_tmp) {

        /* Open all tables */

        if (
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_GLOBAL,   flags | MDBX_INTEGERKEY, &(UniDB->mdbx.dbi_global))                   == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_SETTINGS, flags | MDBX_INTEGERKEY, &(UniDB->mdbx.dbi_settings))                 == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_EVENTS,   flags | MDBX_INTEGERKEY, &(UniDB->mdbx.dbi_events))                   == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open    (txn_tmp, DBTABLE_NAME_CONTACTS, flags | MDBX_INTEGERKEY, &(UniDB->mdbx.dbi_contacts))                 == MDBX_SUCCESS ) &&
            ( mdbx_dbi_open_ex (txn_tmp, DBTABLE_NAME_LOGS,     flags | MDBX_INTEGERKEY, &(UniDB->mdbx.dbi_logs), __log_cmp_helper, NULL) == MDBX_SUCCESS )
        ) {
            uint32_t kVal = 1;
            MDBX_val key = {&kVal, sizeof(kVal)}, data;

            /* Get database header and valide this.
               If header broken recreate it if check CREATENEW flag and don't check READONLY flag
             */

            if ( mdbx_get(txn_tmp, UniDB->mdbx.dbi_global, &key, &data) == MDBX_SUCCESS ) {
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
                    UniDB->header = * hdr;

                } else {
                    trace_dbg ("%s\n", "Return NULL header");
                    return UCM_RET_EMPTY;
                }
            /* Create new header with new tables */
            } else {
                if ((UniDB->flags & UCM_FLAG_DB_CREATENEW) &&
                    (!(UniDB->flags & UCM_FLAG_DB_READONLY)) )
                {
                    UniDB->header.signature = DBSYS_HEADER_SIGNATURE;
                    UniDB->header.version   = MakeLong (DBSYS_VERSION_MAJOR, DBSYS_VERSION_MINOR);
                    data.iov_base = &(UniDB->header), data.iov_len = sizeof(db_header_t);
                    if ( mdbx_put(txn_tmp, UniDB->mdbx.dbi_global, &key, &data, 0) == MDBX_SUCCESS ) {
                        mdbx_db_flush (true);
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

    mdbx_txn_begin (UniDB->mdbx.env, NULL, MDBX_RDONLY, &(UniDB->mdbx.txn_ro));

    mdbx_cursor_open (UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_events  , &(UniDB->mdbx.cur_events));
    mdbx_cursor_open (UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_contacts, &(UniDB->mdbx.cur_contacts));
    mdbx_cursor_open (UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_settings, &(UniDB->mdbx.cur_settings));
    mdbx_cursor_open (UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_logs    , &(UniDB->mdbx.cur_logs));

    mdbx_txn_reset (UniDB->mdbx.txn_ro);

return UCM_RET_SUCCESS;
}

static inline void
__dbcore_release (void)
{
    if (UniDB->sys.mtx)
        app->sys.rwlock_free (UniDB->sys.mtx);
    if (UniDB->sys.clk_flush)
        app->sys.timer_release (UniDB->sys.clk_flush);
}

static inline UCM_RET
__dbcore_init (void)
{
    if ( ( (UniDB->sys.mtx = app->sys.rwlock_create()) == 0 ) &&
         ( (UniDB->sys.clk_flush = app->sys.timer_create()) == 0 )
       ) {
            __dbcore_release();
            return UCM_RET_NONALLOC;
       }
    return UCM_RET_SUCCESS;
}

//static void
//__dbcore_load (uv_fs_t* req) {
//    trace_dbg ("%s\n", "Open database callback");
//    UniDB->flags ^= DB_FLAG_DONTCLOSE;
//}
//
/* ==================================================
        Core API implementation
   ================================================== */

UCM_RET
mdbx_db_open  (const char*  file,
               uint32_t     flags)
{
    UNUSED (file);
    uv_fs_t ufs_access;

    if (__dbcore_init() == UCM_RET_SUCCESS) {
        int r = app->uv.fs_access (&ufs_access, file, R_OK | W_OK, NULL);
        if (r < 0) {
            trace_dbg ("%s: %s\n", file, "fail open");
            return UCM_RET_NOACCESS;
        } else {
            trace_dbg ("%s: %s\n", file, "success open");
            return UCM_RET_SUCCESS;
        }
        app->uv.run(UCM_LOOP_SYSTEM, UV_RUN_ONCE);
        return UCM_RET_SUCCESS;
    } else {
        return UCM_RET_DBERROR;
    }
}

UCM_RET
mdbx_db_close (void)
{
    // TODO
    __dbcore_release();
    return UCM_RET_SUCCESS;
}

void
mdbx_db_flush (bool force)
{
    if (force) {
        mdbx_env_sync (UniDB->mdbx.env, true);
    }
}
