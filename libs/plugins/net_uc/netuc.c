#include "config.h"
#include "ucm.h"

#include <limits.h>
#include <stdlib.h>
#include <time.h>
#include <wchar.h>

const ucm_functions_t* app;

static const char* interface[] = { "proto_ucl", NULL };

static const char**
_get_proto_interface(void)
{
    return interface;
}

static UCM_RET
_run_proto(void)
{
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_proto(void)
{
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2)
{
    // TODO
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

static const ucm_plugproto_t plugin = {.core.oid         = UCM_TYPE_OBJECT_PLUGIN,
                                       .core.info.api    = {.vmajor = UCM_API_MAJOR_VER, .vminor = UCM_API_MINOR_VER},
                                       .core.info.sys    = UCM_TYPE_PLUG_PROTO,
                                       .core.info.vmajor = UCM_VERSION_MAJOR,
                                       .core.info.vminor = UCM_VERSION_MINOR,
                                       .core.info.vpatch = UCM_VERSION_PATCH,
                                       .core.info.flags  = 0,
                                       .core.info.build =
                                           {
                                               .commit   = UCM_BUILD_COMMIT,
                                               .datetime = UCM_BUILD_TIME,
                                               .target   = UCM_BUILD_TARGET,
                                               .compiler = UCM_BUILD_CC,
                                               .options  = UCM_BUILD_OPTS,
                                               .flags    = UCM_BUILD_FLAGS,
                                           },
                                       .core.info.pid       = "unichat_legacy",
                                       .core.info.name      = L"Legacy UniChat 1.46 protocol plugin",
                                       .core.info.developer = L"SkyMaverick",
                                       .core.info.description =
                                           L"Protocol plugin for compatibilty old UniChat 1.46 client programm",
                                       .core.info.copyright = L"Zlib",
                                       .core.info.email     = L"mail@mail.ru",
                                       .core.info.website   = L"http://null.org",

                                       .core.run     = _run_proto,
                                       .core.stop    = _stop_proto,
                                       .core.message = _message,
                                       .flags = UCM_FLAG_NET_IAMSERVERMODE | UCM_FLAG_NET_GROUP | UCM_FLAG_NET_CRYPT |
                                                UCM_FLAG_NET_BROADCAST | UCM_FLAG_NET_MULTYSESSION,
                                       .get_interface = _get_proto_interface};

const ucm_plugproto_t* plucl = &plugin;

LIBUCM_API ucm_plugin_t*
_init_plugin(const ucm_functions_t* api)
{
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
