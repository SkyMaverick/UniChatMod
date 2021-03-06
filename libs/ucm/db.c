#include "api.h"
#include "core.h"
#include "defs.h"
#include "flags.h"
#include "ucm.h"

typedef struct {
    ucm_plugdb_t* worker;
    uintptr_t mtx;
} ucm_db_t;

static ucm_db_t db;

UCM_RET
db_open(const char* aPath) {
    if (aPath == NULL)
        return UCM_RET_WRONGPARAM;

    // zero database structure
    UniAPI->sys.zmemory(&db, sizeof(ucm_db_t));

    //    ucm_plugdb_t** plugins = (ucm_plugdb_t**)(UniAPI->app.plugins_by_type(UCM_TYPE_PLUG_DB));
    uintptr_t pld = UniAPI->app.plugin_get(UCM_TYPE_PLUG_DB);
    if (!pld)
        return UCM_RET_EXCEPTION;

    unsigned ret = 0;
    uv_fs_t ufs_req;

    // init mutex
    db.mtx = UniAPI->sys.mutex_create();
    if (db.mtx <= 0) {
        return UCM_RET_SYSTEM_NOCREATE;
    }

    // Try file access
    int flag_exit = 0;
    do {
        if (get_system_flag(UCM_FLAG_PROFILE_NEW)) {
            UniAPI->sys.fs_fcreate(aPath);
            flag_exit++;
        }
        int r =
            UniAPI->uv.fs_access(UCM_LOOP_SYSTEM(UniAPI), &ufs_req, aPath,
                                 (get_system_flag(UCM_FLAG_PROFILE_RO)) ? R_OK : R_OK | W_OK, NULL);

        if (r < 0) {
            ucm_dtrace("%s: %s\n", aPath, "fail open");
            if (get_system_flag(UCM_FLAG_PROFILE_RO))
                return UCM_RET_SYSTEM_NOACCESS;
            if (get_system_flag(UCM_FLAG_PROFILE_NEW))
                return UCM_RET_BUSY;

            set_system_flag(UCM_FLAG_PROFILE_NEW);
            continue;

        } else
            break;
    } while (!flag_exit);

    UniAPI->uv.run(UCM_LOOP_SYSTEM(UniAPI), UV_RUN_ONCE);
    UniAPI->uv.fs_req_cleanup(&ufs_req);

    // Open database
    int ret_code = UCM_RET_SUCCESS;

    UniAPI->sys.mutex_lock(db.mtx);
    do {
        ucm_plugdb_t* pl = (ucm_plugdb_t*)U_PLUGIN(pld);

        if (pl->db_open != NULL) {
            ret_code = pl->db_open();
            if (ret_code == UCM_RET_SUCCESS) {
                db.worker = pl;
            } else {
                switch (ret) {
                case UCM_RET_DATABASE_BADFORMAT:
                    // TODO check database
                    break;
                case UCM_RET_DATABASE_BADVERSION:
                    // TODO remake database
                    break;
                }
            }
        } else {
            ucm_dtrace("%s: %s\n", pl->core.info.name, "method db_open() not implemented");
        }
        pld = UniAPI->app.plugin_next(pld);
    } while (pld);

    UniAPI->sys.mutex_unlock(db.mtx);

    if (db.worker == NULL) {
        ret_code = UCM_RET_DATABASE_BADFORMAT;
    } else {
        UniAPI->app.mainloop_msg_send(UCM_SIG_DBLOAD_SUCCESS, (uintptr_t)db.worker, ret_code, 0);
    }

    return ret_code;
}

UCM_RET
db_close(void) {
    UniAPI->sys.mutex_lock(db.mtx);
    if (db.worker) {
        db.worker->db_flush(true);
        db.worker->db_close();
    }
    UniAPI->sys.mutex_unlock(db.mtx);
    UniAPI->sys.mutex_free(db.mtx);
    return UCM_RET_SUCCESS;
}
