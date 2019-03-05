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
#include "db_mdbx_base.h"

const ucm_functions_t* app;

db_object_t* UCM_DB = NULL;

// ######################################################################
//      INTERNAL PLUGIN SERVICE FUNCTIONS
// ######################################################################

void
_assert_func (const MDBX_env *env,
              const char     *msg,
              const char     *function,
              unsigned       line)
{
    UNUSED (env);
    trace_err(_("%s. Assert in %s (line: %d)\n"), msg, function, line);
}
// ######################################################################
//      STANDART PLUGIN API IMPLEMENTATION
// ######################################################################

static UCM_RET
_run_dbmdbx (void)
{
    UCM_DB = app->sys.zmalloc (sizeof(db_object_t) + (UCM_PATH_MAX * sizeof(char)));
    if ( UCM_DB == NULL )
        return UCM_RET_NONALLOC;
    UCM_DB->mtx = app->sys.rwlock_create ();
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_dbmdbx (void)
{
    app->sys.rwlock_free ( UCM_DB->mtx );
    ucm_free_null (UCM_DB);
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id,
         uintptr_t ctx,
         uint32_t x1,
         uint32_t x2)
{
    //TODO
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

// ######################################################################
//      DATABASE PLUGIN API IMPLEMENTATION
// ######################################################################

static UCM_RET
mdbx_db_open  (const char*  file,
               uint32_t     flags)
{
    int ret = UCM_RET_SUCCESS;

    app->sys.rwlock_wlock (UCM_DB->mtx);

    snprintf (UCM_DB->faPath, UCM_PATH_MAX, "%s", file);
    UCM_DB->flags   =   flags;
    ret = db_open(UCM_DB);

    app->sys.rwlock_unlock (UCM_DB->mtx);

    return ret;
}

static UCM_RET
mdbx_db_close (void)
{
    // TODO
    return UCM_RET_SUCCESS;
}

static UCM_RET
mdbx_db_flush (void)
{
    return UCM_RET_SUCCESS;
}

static ucm_plugdb_t plugin = {
    .core.oid                = UCM_TYPE_OBJECT_PLUGIN,
    .core.info.api           =
    {
        .vmajor              = UCM_API_MAJOR_VER,
        .vminor              = UCM_API_MINOR_VER
    },
    .core.info.sys           = UCM_TYPE_PLUG_DB,
    .core.info.vmajor        = UCM_VERSION_MAJOR,
    .core.info.vminor        = UCM_VERSION_MINOR,
    .core.info.vpatch        = UCM_VERSION_PATCH,
    .core.info.flags         = 0,
    .core.info.build         =
    {
        .commit              = UCM_BUILD_COMMIT,
        .datetime            = UCM_BUILD_TIME,
        .target              = UCM_BUILD_TARGET,
        .compiler            = UCM_BUILD_CC,
        .options             = UCM_BUILD_OPTS,
        .flags               = UCM_BUILD_FLAGS,
    },
    .core.info.pid           = L"dbmdbx",
    .core.info.name          = L"Storage mdbx plugin",
    .core.info.developer     = L"SkyMaverick",
    .core.info.description   = L"System standart storage plugin (based on libmdbx).",
    .core.info.copyright     = L"Zlib + ReOpenLDAP",
    .core.info.email         = L"mail@mail.ru",
    .core.info.website       = L"http://null.org",

    .core.run                = _run_dbmdbx,
    .core.stop               = _stop_dbmdbx,
    .core.message            = _message,

    .db_open                 = mdbx_db_open,
    .db_check                = NULL,
    .db_flush                = mdbx_db_flush,
    .db_close                = mdbx_db_close,
};

const ucm_plugdb_t* pldb = &plugin;

LIBUCM_API ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
