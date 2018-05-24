#include "ucm.h"
#include "api.h"
#include "core.h"
#include "config.h"

static ucm_plugin_t core_lib = {
    .info.api.vmajor  = UCM_API_MAJOR_VER,
    .info.api.vminor  = UCM_API_MINOR_VER,
    .info.type        = 0,
    .info.vmajor      = UCM_VERSION_MAJOR, //TODO
    .info.vminor      = UCM_VERSION_MINOR,
    .info.vpatch      = UCM_VERSION_PATCH,
    .info.build       =
    {
        .commit       = UCM_BUILD_COMMIT,
        .datetime     = UCM_BUILD_TIME,
        .target       = UCM_BUILD_TARGET,
        .compiler     = UCM_BUILD_CC,
        .options      = UCM_BUILD_OPTS,
        .flags        = UCM_BUILD_FLAGS,
    },
    .info.flags       = 0,

    .info.pid         = "ucm_core",
    .info.name        = "UniChatMod core plugin",
    .info.developer   = "SkyMaverick",
    .info.description = "UniChatMod core library plugin",
    .info.copyright   = "Zlib",
    .info.email       = "",
    .info.website     = "",

    .run         = NULL,
    .stop        = NULL,
    .message     = NULL
};

ucm_plugin_t* lib_core = &core_lib;
