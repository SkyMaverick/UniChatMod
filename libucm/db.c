#include "ucm.h"
#include "api.h"
#include "core.h"
#include "defs.h"

typedef struct {
    ucm_plugdb_t* worker;
} ucm_db_t;

static ucm_db_t db;

UCM_RET
db_open ( char*      aPath,
          uint32_t   flags )
{
    if (aPath == NULL)
        return UCM_RET_WRONGPARAM;

    ucm_plugdb_t** plugins = (ucm_plugdb_t**) (UniAPI->get_plugins_db());
    unsigned ret = 0;

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
    return (db.worker == NULL) ? UCM_RET_NOOBJECT : UCM_RET_SUCCESS;
}

UCM_RET
db_close (void)
{
    if (db.worker) {
        db.worker->db_flush();
        db.worker->db_close();
    }
    return UCM_RET_SUCCESS;
}


