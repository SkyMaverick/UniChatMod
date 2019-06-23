#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <wchar.h>

#include "ucm.h"
#include "config.h"

const ucm_functions_t* app;
static const ucm_plugin_t plugin;


static void
cb_logger_function (ucm_plugin_t*   plugin,
                    uint32_t        type,
                    const char*     txt,
                    void*           ctx)
{
    fprintf (stdout, "\t[%s] %s:%d\t%s", "LOGGER", "type", type, txt);
}

static UCM_RET
_run_logger (void)
{
    app->app.logger_connect (cb_logger_function, NULL);
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_logger (void)
{
    app->app.logger_disconnect (cb_logger_function);
    return UCM_RET_SUCCESS;
}

static void
_message (uint32_t id,
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

static const ucm_plugin_t plugin = {
    .oid                = UCM_TYPE_OBJECT_PLUGIN,
    .info.api           =
    {
        .vmajor         = UCM_API_MAJOR_VER,
        .vminor         = UCM_API_MINOR_VER
    },
    .info.sys           = UCM_TYPE_PLUG_STUFF,
    .info.vmajor        = UCM_VERSION_MAJOR,
    .info.vminor        = UCM_VERSION_MINOR,
    .info.vpatch        = UCM_VERSION_PATCH,
    .info.flags         = 0,
    .info.build         =
    {
        .commit         = UCM_BUILD_COMMIT,
        .datetime       = UCM_BUILD_TIME,
        .target         = UCM_BUILD_TARGET,
        .compiler       = UCM_BUILD_CC,
        .options        = UCM_BUILD_OPTS,
        .flags          = UCM_BUILD_FLAGS,
    },
    .info.pid           = "txt_log",
    .info.name          = L"Text logger",
    .info.developer     = L"SkyMaverick",
    .info.description   = L"Simple text file output logger",
    .info.copyright     = L"Zlib",
    .info.email         = L"mail@mail.ru",
    .info.website       = L"http://null.org",

    .run                = _run_logger,
    .stop               = _stop_logger,
    .message            = _message,
};

LIBUCM_API ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
