#include <stdlib.h>
#include <stdio.h>

#include "ucm.h"
#include "api.h"
#include "threading.h"
#include "db_mdbx.h"
#include "libmdbx/mdbx.h"

db_object_t* UCM_DB;
const ucm_functions_t* app;

#define DB UCM_DB->mdbx

static UCM_RET
db_map ()
{
    mdbx_env_create (&(DB.env));
    mdbx_env_set_maxdbs (DB.env, 10);
    mdbx_env_set_maxreaders (DB.env, 244);
    mdbx_env_set_userctx (DB.env, UCM_DB);
    mdbx_env_set_assert (DB.env, _assert_func);

    int rc = mdbx_env_set_geometry (DB.env,
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
    if (UCM_DB->flags | DB_FLAG_READONLY) {
        mode |= MDBX_RDONLY;
    }

    return ( mdbx_env_open (DB.env, UCM_DB->file_apath, mode, 0664) != MDBX_SUCCESS )
           ? UCM_RET_NOACCESS : UCM_RET_SUCCESS;
}

UCM_RET
db_open (char* fname)
{
    if ( db_map() == UCM_RET_SUCCESS ) {
        // TODO
    }
    return UCM_RET_SUCCESS;
}

UCM_RET
db_flush ()
{
    return UCM_RET_SUCCESS;
}

UCM_RET
db_close ()
{
    return UCM_RET_SUCCESS;
}

UCM_RET
db_check ()
{
    return UCM_RET_SUCCESS;
}

UCM_RET
db_backup (char* bfName)
{
    return UCM_RET_SUCCESS;
}

#undef DB
