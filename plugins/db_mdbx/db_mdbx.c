#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include "ucm.h"
#include "config.h"
#include "libmdbx/mdbx.h"

static const ucm_functions_t* app;

static ucm_dbplugin_t plugin;

#define trace_dbg(fmt, ...) {app->log(&plugin,UCM_LOG_DEBUG,fmt,__VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log{&plugin,UCM_LOG_INFO,fmt,__VA_ARGS__};}
#define trace_err(fmt, ...) {app->log{&plugin,UCM_LOG_ERROR,fmt,__VA_ARGS__};}

static UCM_RET
_run_dbmdbx (void)
{
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_dbmdbx (void)
{
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id,
         uintptr_t ctx,
         uint32_t x1,
         uint32_t x2)
{
    //TODO
}

static ucm_dbplugin_t plugin = {
    .core.info.api           =
    {
        .vmajor              = UCM_API_MAJOR_VER,
        .vminor              = UCM_API_MINOR_VER
    },
    .core.info.sys           = UCM_PLUG_DB,
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
    .core.info.pid           = "dbmdbx",
    .core.info.name          = "Storage mdbx plugin",
    .core.info.developer     = "SkyMaverick",
    .core.info.description   = "System standart storage plugin (based on libmdbx).",
    .core.info.copyright     = "Zlib + ReOpenLDAP",
    .core.info.email         = "mail@mail.ru",
    .core.info.website       = "http://null.org",

    .core.run                = _run_dbmdbx,
    .core.stop               = _stop_dbmdbx,
    .core.message            = _message,

    .db_open                 = NULL,
    .db_check                = NULL,
    .db_flush                = NULL,
    .db_close                = NULL,

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
