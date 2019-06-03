#include "ucm.h"
#include "api.h"
#include "core.h"
#include "defs.h"

typedef struct {
    ucm_plugdb_t* worker;
    uintptr_t   mtx;
} ucm_db_t;

static ucm_db_t db;

UCM_RET
db_open ( const char* aPath,
          uint32_t    flags )
{
    if (aPath == NULL)
        return UCM_RET_WRONGPARAM;

    // zero database structure
    UniAPI->sys.zmemory(&db, sizeof(ucm_db_t));

    ucm_plugdb_t** plugins = (ucm_plugdb_t**) (UniAPI->app.get_plugins_db());
    unsigned ret = 0;
    uv_fs_t ufs_req;

    // init mutex
    db.mtx  = UniAPI->sys.mutex_create();
    if (db.mtx <= 0) {
        return UCM_RET_EXCEPTION;
    }

    // Try file access
    int flag_exit = 0;
    do {
        if (flags & UCM_FLAG_NEWPROF) {
            UniAPI->sys.fs_fcreate(aPath);
            flag_exit ++;
        }
        int r = UniAPI->uv.fs_access (&ufs_req, aPath,
                                      (flags & UCM_FLAG_ROPROF) ?
                                               R_OK : R_OK | W_OK,
                                      NULL);

        if (r < 0) {
            ucm_dtrace ("%s: %s\n", aPath, "fail open");
//            UniAPI->uv.run(UCM_LOOP_SYSTEM, UV_RUN_ONCE);
//            UniAPI->uv.fs_req_cleanup(&ufs_req);

            if (flags & UCM_FLAG_ROPROF)
                return UCM_RET_NOACCESS;
            if (flags & UCM_FLAG_NEWPROF)
                return UCM_RET_BUSY;

            flags |= UCM_FLAG_NEWPROF;
            continue;

        } else break;
    } while (!flag_exit);

    UniAPI->uv.run(UCM_LOOP_SYSTEM, UV_RUN_ONCE);
    UniAPI->uv.fs_req_cleanup(&ufs_req);

    // Open database
    UniAPI->sys.mutex_lock(db.mtx);
    while (*plugins) {
        if ( (*plugins)->db_open != NULL ) {
            if ( ( ret = (*plugins)->db_open(flags) ) == UCM_RET_SUCCESS ) {
                db.worker = *plugins;
            } else {
                switch (ret) {
                    case UCM_RET_INVALID:
                        {
                            // TODO check database
                            break;
                        }
                    case UCM_RET_BADVERSION:
                        {
                            // TODO remake database
                            break;
                        }
                }
            }
        } else {
            ucm_dtrace ("%s: %s\n", (*plugins)->core.info.pid, "method db_open() not implemented");
        }
        plugins++;
    }
    UniAPI->sys.mutex_unlock(db.mtx);
    return (db.worker == NULL) ? UCM_RET_NOOBJECT : UCM_RET_SUCCESS;
}

UCM_RET
db_close (void)
{
    UniAPI->sys.mutex_lock(db.mtx);
    if (db.worker) {
        db.worker->db_flush(true);
        db.worker->db_close();
    }
    UniAPI->sys.mutex_unlock(db.mtx);
    UniAPI->sys.mutex_free(db.mtx);
    return UCM_RET_SUCCESS;
}
