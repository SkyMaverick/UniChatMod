#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "ucm.h"
#include "alloc.h"
#include "config.h"
#include "gettext.h"
#include "libmdbx/mdbx.h"

#include "db_mdbx.h"

const ucm_functions_t* app;
static ucm_dbplugin_t plugin;

#define trace_dbg(fmt, ...) {app->log ( (ucm_plugin_t*)(&plugin), UCM_TYPE_LOG_DEBUG, fmt, __VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log ( (ucm_plugin_t*)(&plugin), UCM_TYPE_LOG_INFO,  fmt, __VA_ARGS__);}
#define trace_err(fmt, ...) {app->log ( (ucm_plugin_t*)(&plugin), UCM_TYPE_LOG_ERROR, fmt, __VA_ARGS__);}

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
    UCM_DB = ucm_zmalloc (sizeof(db_object_t));
    if ( UCM_DB == NULL ) 
        return UCM_RET_NONALLOC;
    UCM_DB->mtx = app->rwlock_create ();
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_dbmdbx (void)
{
    app->rwlock_free ( UCM_DB->mtx );
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
mdbx_db_open  (char*    file,
               uint32_t flags)
{
    UNUSED(flags);
    snprintf (UCM_DB->file_apath, PATH_MAX, "%s", file);

    return UCM_RET_SUCCESS;
}

static UCM_RET
mdbx_db_close (void)
{
    // TODO
    return UCM_RET_SUCCESS;
}

static ucm_dbplugin_t plugin = {
    .core.info.api           =
    {
        .vmajor              = UCM_API_MAJOR_VER,
        .vminor              = UCM_API_MINOR_VER
    },
    .core.info.sys           = UCM_TYPE_PLUG_DB,
    .core.info.vmajor        = UCM_VERSION_MAJOR,
    .core.info.vminor        = UCM_VERSION_MINOR,
    .core.info.vpatch        = UCM_VERSION_PATCH,
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
    .db_flush                = NULL,
    .db_close                = mdbx_db_close,

    .get_int                 = NULL,
    .get_int64               = NULL,
    .get_float               = NULL,
    .get_str                 = NULL,
    .set_int                 = NULL,
    .set_int64               = NULL,
    .set_float               = NULL,
    .set_str                 = NULL,
    .item_del                = NULL
};

ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
