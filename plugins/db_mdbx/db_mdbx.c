#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include "ucm.h"
#include "config.h"
#include "libmdbx/mdbx.h"

static const ucm_functions_t* app;
static ucm_plugin_t plugin;

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

}

static ucm_plugin_t plugin = {
    .info.api    =
    {
        .vmajor = UCM_API_MAJOR_VER,
        .vminor = UCM_API_MINOR_VER
    },
    .info.sys   = UCM_PLUG_DB,
    .info.vmajor = UCM_VERSION_MAJOR,
    .info.vminor = UCM_VERSION_MINOR,
    .info.vpatch = UCM_VERSION_PATCH,
    .info.build  =
    {
        .commit       = UCM_BUILD_COMMIT,
        .datetime     = UCM_BUILD_TIME,
        .target       = UCM_BUILD_TARGET,
        .compiler     = UCM_BUILD_CC,
        .options      = UCM_BUILD_OPTS,
        .flags        = UCM_BUILD_FLAGS,
    },
    .info.pid = "dbmdbx",
    .info.name = "Storage mdbx plugin",
    .info.developer = "SkyMaverick",
    .info.description = "System standart storage plugin (based on libmdbx).",
    .info.copyright = "Zlib + ReOpenLDAP",
    .info.email = "mail@mail.ru",
    .info.website = "http://null.org",
    .run = _run_dbmdbx,
    .stop = _stop_dbmdbx,
    .message = _message
};

ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return &plugin;
}
