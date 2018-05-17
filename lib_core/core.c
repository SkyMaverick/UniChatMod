#include "ucm.h"
#include "api.h"
#include "core.h"

static ucm_plugin_t core_lib = {
    .api.vmajor  = UCM_API_MAJOR_VER,
    .api.vminor  = UCM_API_MINOR_VER,
    .type        = 0,
    .vmajor      = 0, //TODO
    .vminor      = 0,
    .flags       = 0,

    .pid         = "ucm_core",
    .name        = "UniChatMod core plugin",
    .developer   = "SkyMaverick",
    .description = "UniChatMod core library plugin",
    .copyright   = "Zlib",
    .email       = "",
    .website     = "",

    .run         = NULL,
    .stop        = NULL,
    .message     = NULL
};

ucm_plugin_t* lib_core = &core_lib;
