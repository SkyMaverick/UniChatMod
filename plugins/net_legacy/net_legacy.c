#include <wchar.h>

#include "ucm.h"
#include "alloc.h"
#include "config.h"

const ucm_functions_t* app;
static ucm_pclplugin_t plugin;

static UCM_RET
_run_proto (void)
{
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_proto (void)
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
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

static ucm_pclplugin_t plugin = {
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
    .core.info.pid           = L"unichat_legacy",
    .core.info.name          = L"Legacy UniChat 1.46 protocol plugin",
    .core.info.developer     = L"SkyMaverick",
    .core.info.description   = L"Protocol plugin for compatibilty old UniChat 1.46 client programm",
    .core.info.copyright     = L"Zlib",
    .core.info.email         = L"mail@mail.ru",
    .core.info.website       = L"http://null.org",

    .core.run                = _run_proto,
    .core.stop               = _stop_proto,
    .core.message            = _message,
    .flags                   =  UCM_FLAG_NET_IAMSERVERMODE
                              | UCM_FLAG_NET_GROUP
                              | UCM_FLAG_NET_CRYPT
                              | UCM_FLAG_NET_BROADCAST
                              | UCM_FLAG_NET_MULTYSESSION
};

ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
