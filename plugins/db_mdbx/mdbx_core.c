#include "mdbx_core.h"
#include "ucm.h"

#include "config.h"
#include "db_mdbx.h"
#include "gettext.h"
#include "mdbx.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

static int
__log_cmp_helper(const MDBX_val* a, const MDBX_val* b)
{
    // TODO make
    UNUSED(a);
    UNUSED(b);

    return 0;
}

static void
cb_mdbx_timer(uv_timer_t* timer)
{
    mdbx_db_flush(true);
}

static void*
mdbx_autosync(void* ctx)
{
    app->uv.timer_start((uv_timer_t*)ctx, cb_mdbx_timer, 0, 50);
    app->uv.run(UCM_LOOP_SYSTEM(app), UV_RUN_DEFAULT);
    return NULL;
}

static void
cb_create_backup(uv_fs_t* open_req)
{
    // TODO
    if (open_req->result >= 0) {
        trace_inf("%s: %s\n", "Create backup on file", open_req->file) uv_fs_t close_req;
#if defined(UCM_OS_WINDOWS)
        int res = mdbx_env_copy2fd(UniDB->mdbx.env, open_req->result, MDBX_CP_COMPACT);
#else
        int res = mdbx_env_copy2fd(UniDB->mdbx.env, open_req->result, MDBX_CP_COMPACT);
#endif
        if (res == MDBX_SUCCESS) {
            // TODO Send message OK async operation
        } else {
            // TODO Send message FAIL async operation
        }

        app->uv.fs_close(UCM_LOOP_SYSTEM(app), &close_req, open_req->result, NULL);
        app->uv.fs_req_cleanup(&close_req);
    } else {
        // TODO Send message FAIL async operation
    }
}

/* ==================================================
        Core API implementation
   ================================================== */

static UCM_RET
mdbx_core_map(void)
{
    mdbx_env_create(&(UniDB->mdbx.env));
    mdbx_env_set_maxdbs(UniDB->mdbx.env, 10);
    mdbx_env_set_maxreaders(UniDB->mdbx.env, 244);
    mdbx_env_set_userctx(UniDB->mdbx.env, UniDB);
    mdbx_env_set_assert(UniDB->mdbx.env, _assert_func);

    int rc = mdbx_env_set_geometry(UniDB->mdbx.env, -1, 1ul << 20, 512ul << 20, 1ul << 20,
                                   512ul << 10, -1);
    if (rc != MDBX_SUCCESS)
        return UCM_RET_DATABASE_BADINIT;

    unsigned int mode =
      MDBX_NOSUBDIR | MDBX_MAPASYNC | MDBX_WRITEMAP | MDBX_NOSYNC | MDBX_COALESCE | MDBX_EXCLUSIVE;
    if (UniDB->flags & UCM_FLAG_ROPROF) {
        trace_dbg("%s\n", "Set read-only database flag");
        mode |= MDBX_RDONLY;
    }

    return (mdbx_env_open(UniDB->mdbx.env, app->app.get_store_path(), mode, 0664) != MDBX_SUCCESS)
             ? UCM_RET_DATABASE_BADINIT
             : UCM_RET_SUCCESS;
}

static inline UCM_RET
mdbx_core_load(void)
{
    unsigned flags    = (UniDB->flags & UCM_FLAG_ROPROF) ? 0 : MDBX_CREATE;
    MDBX_txn* txn_tmp = StartTxn(UniDB);
    if (txn_tmp) {
        /* Open all tables */

        if ((mdbx_dbi_open(txn_tmp, DBTABLE_NAME_GLOBAL, flags | MDBX_INTEGERKEY,
                           &(UniDB->mdbx.dbi_global)) == MDBX_SUCCESS) &&
            (mdbx_dbi_open(txn_tmp, DBTABLE_NAME_SETTINGS, flags | MDBX_INTEGERKEY,
                           &(UniDB->mdbx.dbi_settings)) == MDBX_SUCCESS) &&
            (mdbx_dbi_open(txn_tmp, DBTABLE_NAME_EVENTS, flags | MDBX_INTEGERKEY,
                           &(UniDB->mdbx.dbi_events)) == MDBX_SUCCESS) &&
            (mdbx_dbi_open(txn_tmp, DBTABLE_NAME_CONTACTS, flags | MDBX_INTEGERKEY,
                           &(UniDB->mdbx.dbi_contacts)) == MDBX_SUCCESS) &&
            (mdbx_dbi_open_ex(txn_tmp, DBTABLE_NAME_LOGS, flags | MDBX_INTEGERKEY,
                              &(UniDB->mdbx.dbi_logs), __log_cmp_helper, NULL) == MDBX_SUCCESS)) {
            uint32_t kVal = 1;
            MDBX_val key  = { &kVal, sizeof(kVal) }, data;

            /* Get database header and valide this.
               If header broken recreate it if check CREATENEW flag and don't
               check READONLY flag
             */

            if (mdbx_get(txn_tmp, UniDB->mdbx.dbi_global, &key, &data) == MDBX_SUCCESS) {
                // TODO
                const db_header_t* hdr = (const db_header_t*)data.iov_base;
                if (hdr) {
                    /* Databse signature valide  */
                    if (hdr->signature != DBSYS_HEADER_SIGNATURE) {
                        trace_err("%s\n", "Header signature not found. Maybe this not "
                                          "UCM database.");
                        return UCM_RET_DATABASE_BADFORMAT;
                    }
                    /*  Database format version valide */
                    if (hdr->version != MakeLong(DBSYS_VERSION_MAJOR, DBSYS_VERSION_MINOR)) {
                        trace_err("%s: %d \n", "Bad database major version. Need convert to",
                                  DBSYS_VERSION_MAJOR);
                        return UCM_RET_DATABASE_BADVERSION;
                    }
                    UniDB->header = *hdr;
                    trace_inf(
                      "%s (%S): %zu.%zu\n", "Database version", UniDB->plugin.core.info.name,
                      ((UniDB->header.version >> 32) & 0x00FF), (UniDB->header.version & 0x00FF));

                } else {
                    trace_dbg("%s\n", "Return NULL header");
                    return UCM_RET_DATABASE_BADMETADATA;
                }
                /* Create new header with new tables */
            } else {
                if ((UniDB->flags & UCM_FLAG_NEWPROF) && (!(UniDB->flags & UCM_FLAG_ROPROF))) {
                    UniDB->header.signature = DBSYS_HEADER_SIGNATURE;
                    UniDB->header.version   = MakeLong(DBSYS_VERSION_MAJOR, DBSYS_VERSION_MINOR);
                    data.iov_base = &(UniDB->header), data.iov_len = sizeof(db_header_t);
                    if (mdbx_put(txn_tmp, UniDB->mdbx.dbi_global, &key, &data, 0) == MDBX_SUCCESS) {
                        mdbx_db_flush(true);
                    } else {
                        trace_err("%s: %s\n", "Create database error", mdbx_strerror(errno));
                        return UCM_RET_DATABASE_NOTCHANGE;
                    }
                } else {
                    trace_err("%s : %s\n", "Open database error", mdbx_strerror(errno));
                    return UCM_RET_DATABASE_NOTCHANGE;
                }
            }
        }
        mdbx_txn_commit(txn_tmp);
    }

    mdbx_txn_begin(UniDB->mdbx.env, NULL, MDBX_RDONLY, &(UniDB->mdbx.txn_ro));

    mdbx_cursor_open(UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_events, &(UniDB->mdbx.cur_events));
    mdbx_cursor_open(UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_contacts, &(UniDB->mdbx.cur_contacts));
    mdbx_cursor_open(UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_settings, &(UniDB->mdbx.cur_settings));
    mdbx_cursor_open(UniDB->mdbx.txn_ro, UniDB->mdbx.dbi_logs, &(UniDB->mdbx.cur_logs));

    mdbx_txn_reset(UniDB->mdbx.txn_ro);

    return UCM_RET_SUCCESS;
}

static void
mdbx_core_unload(void)
{
    trace_dbg("%s\n", "Unload MDBX");

    mdbx_txn_commit(UniDB->mdbx.txn_ro);

    mdbx_cursor_close(UniDB->mdbx.cur_events);
    mdbx_cursor_close(UniDB->mdbx.cur_contacts);
    mdbx_cursor_close(UniDB->mdbx.cur_settings);
    mdbx_cursor_close(UniDB->mdbx.cur_logs);

    mdbx_env_close(UniDB->mdbx.env);
}

static inline void
dbi_core_release(void)
{
    //    app->sys.timer_release (UniDB->sys.tmFlush);
    mdbx_core_unload();
}

static inline UCM_RET
dbi_core_init(void)
{
    UniDB->sys.tick = app->sys.zmalloc(sizeof(uv_timer_t));
    if (UniDB->sys.tick == NULL)
        return UCM_RET_SYSTEM_NOMEMORY;

    if (app->uv.timer_init(UCM_LOOP_SYSTEM(app), UniDB->sys.tick)) {
        app->sys.free(UniDB->sys.tick);
        return UCM_RET_SYSTEM_NOCREATE;
    }
    return UCM_RET_SUCCESS;
}

/* ==================================================
        Core API implementation
   ================================================== */

UCM_RET
mdbx_db_open(uint32_t flags)
{
    UniDB->flags = flags;

    int ret_code = dbi_core_init();
    if (ret_code != UCM_RET_SUCCESS)
        return ret_code;

    ret_code = mdbx_core_map();
    if (ret_code != UCM_RET_SUCCESS)
        return ret_code;

    ret_code = mdbx_core_load();
    if (ret_code != UCM_RET_SUCCESS)
        return ret_code;

    app->sys.thread_create(mdbx_autosync, (void*)(UniDB->sys.tick));

    return UCM_RET_SUCCESS;
}

UCM_RET
mdbx_db_close(void)
{
    // TODO
    app->uv.timer_stop(UniDB->sys.tick);
    app->uv.run(UCM_LOOP_SYSTEM(app), UV_RUN_DEFAULT);

    dbi_core_release();
    return UCM_RET_SUCCESS;
}

void
mdbx_db_flush(bool force)
{
    mdbx_env_sync(UniDB->mdbx.env, true);
    if (force) {
        // TODO
    }
}

UCM_RET
mdbx_db_backup(char* path)
{
    uv_fs_t open_req;
    if (app->uv.fs_open(UCM_LOOP_SYSTEM(app), &open_req, path, O_WRONLY | O_CREAT, 0664,
                        cb_create_backup) > 0) {
        trace_err("%s: %s\n", "Backup fail. Don't create file", path);
        app->uv.fs_req_cleanup(&open_req);
        return UCM_RET_SYSTEM_NOACCESS;
    }
    app->uv.run(UCM_LOOP_SYSTEM(app), UV_RUN_DEFAULT);
    app->uv.fs_req_cleanup(&open_req);
    return UCM_RET_SUCCESS;
}
