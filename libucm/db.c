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

    UniAPI->sys.zmemory(&db, sizeof(ucm_db_t));

    ucm_plugdb_t** plugins = (ucm_plugdb_t**) (UniAPI->app.get_plugins_db());
    unsigned ret = 0;

    db.mtx  = UniAPI->sys.mutex_create();
    if (db.mtx <= 0) {
        return UCM_RET_EXCEPTION;
    }
    
    UniAPI->sys.mutex_lock(db.mtx);
    while (*plugins) {
        if ( (*plugins)->db_open != NULL ) {
            if ( ( ret = (*plugins)->db_open(aPath, flags) ) == UCM_RET_SUCCESS ) {
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
            ucm_dtrace ("%S: %s\n", (*plugins)->core.info.pid, "method db_open() not implemented");
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
