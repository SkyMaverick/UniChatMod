#include "ucm.h"
#include "api.h"
#include "defs.h"
#include "config.h"
#include "gettext.h"

#include "core.h"
#include "mainloop.h"
#include "plugmgr.h"
#include "evhook.h"
#include "logger.h"

static uintptr_t tid_loop_core = 0;

static void
loop_core (void* ctx)
{
    uint32_t  id;
    uintptr_t lctx;
    uint32_t  x1;
    uint32_t  x2;
    unsigned  term = 0;

    UNUSED(ctx);

    while(1) {
        while ( ucm_mloop_pop(&id, &lctx, &x1, &x2) == UCM_RET_SUCCESS ) {
            // hook events for shost applications
            hooks_event(id, lctx, x1, x2);
            // send message to all plugins
            plugins_message_dispatch(&id, &lctx, &x1, &x2);

            switch (id) {
                case UCM_EV_TERM:
                    {
                        term = 1;
                        ucm_dtrace ("%s\n", "Catch TERM message. Core loop exit.");
                        break;
                    }
            }
        }
        if (term) break;
        ucm_mloop_wait();
    }
}

static UCM_RET
_run_core (void)
{
    // start main message loop
    if ( ucm_mloop_init(UCM_DEF_MQ_LIMIT) == UCM_RET_SUCCESS ) {

        log_init();
        hooks_event_init();

        tid_loop_core = ucm_global_api->thread_create(loop_core, NULL);
        plugins_run_all();
    }
    ucm_dtrace("%s: %s\n", _("Success start UniChatMod core ver."), UCM_VERSION);
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_core (void)
{
    // send TERM message for stop systems and plugins prepare
    ucm_global_api->mainloop_msg_send(UCM_EV_TERM, (uintptr_t)ucm_global_core, 0, 0);
    // stop all plugins
    plugins_stop_all();
    // stop main message loop
    ucm_global_api->thread_join(tid_loop_core);
    ucm_mloop_free();

    log_release();
    hooks_event_release();

    return UCM_RET_SUCCESS;
}

static void
_message_core(uint32_t id,
              uintptr_t ctx,
              uint32_t x1,
              uint32_t x2)

{
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
    switch (id) {
        // TODO
    }
}

static ucm_plugin_t core_lib = {
    .info.api.vmajor  = UCM_API_MAJOR_VER,
    .info.api.vminor  = UCM_API_MINOR_VER,
    .info.sys         = 0,
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

    .run         = _run_core,
    .stop        = _stop_core,
    .message     = _message_core
};

ucm_plugin_t* ucm_global_core = &core_lib;
