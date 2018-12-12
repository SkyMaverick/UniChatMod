#include <wchar.h>

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
#include "db.h"

typedef struct {
    ucm_plugin_t        base;           // base plugin functionality (start/stop/mq)
} ucm_core_t;

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
            // hook events for host applications
            hooks_event(id, lctx, x1, x2);
            // send message to all plugins
            plugins_message_dispatch(&id, &lctx, &x1, &x2);

            switch (id) {
                case UCM_EVENT_TERM:
                    {
                        term = 1;
                        ucm_dtrace ("%s\n", "Catch TERM message. Core loop exit.");
                        break;
                    }
                case UCM_EVENT_START_GUI:
                    {
                        ucm_dtrace ("%s: %S\n", "Catch start GUI signal", (wchar_t*)lctx);
                        // TODO start select GUI plugin or default
                        break;
                    }
            }
        }
        if (term) break;
        ucm_mloop_wait();
    }
}

static UCM_RET
_stop_core (void)
{

    if (tid_loop_core > 0) {
        UniAPI->mainloop_msg_send(UCM_EVENT_TERM, (uintptr_t)ucm_core, 0, 0);
        UniAPI->thread_join(tid_loop_core);
    }
    db_close();
    plugins_stop_all();
    ucm_mloop_free();

    log_release();
    hooks_event_release();

    return UCM_RET_SUCCESS;
}

static UCM_RET
_run_core (void)
{
    extern wchar_t ucm_path_store [UCM_PATH_MAX];
    char aPath [UCM_PATH_MAX];

    if ( wcstombs(aPath, ucm_path_store, UCM_PATH_MAX) <= 0) {
        ucm_etrace ("%s: %s\n", "Don't parse database file path", aPath);
        return UCM_RET_WRONGPARAM;
    }
    // start main message loop
    if ( ucm_mloop_init(UCM_DEF_MQ_LIMIT) == UCM_RET_SUCCESS ) {

        log_init();
        hooks_event_init();
        plugins_run_all();

        if ( db_open(aPath, 0) != UCM_RET_SUCCESS) {
            ucm_etrace ("%s: %s\n", "Couldn't open this database file", aPath);
            _stop_core();
            return UCM_RET_WRONGPARAM;
        }

        tid_loop_core = UniAPI->thread_create(loop_core, NULL);
    }
    ucm_dtrace("%s: %s\n", _("Success start UniChatMod core ver."), UCM_VERSION);
    return UCM_RET_SUCCESS;
}

static void
_message_core(uint32_t id,
              uintptr_t ctx,
              uint32_t x1,
              uint32_t x2)

{
    switch (id) {
        // TODO
    };
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
    switch (id) {
        // TODO
    }
}

static ucm_core_t core_lib = {
    .base.oid              = UCM_TYPE_OBJECT_PLUGIN,
    .base.info.api.vmajor  = UCM_API_MAJOR_VER,
    .base.info.api.vminor  = UCM_API_MINOR_VER,
    .base.info.sys         = 0,
    .base.info.vmajor      = UCM_VERSION_MAJOR, //TODO
    .base.info.vminor      = UCM_VERSION_MINOR,
    .base.info.vpatch      = UCM_VERSION_PATCH,
    .base.info.build       =
    {
        .commit       = UCM_BUILD_COMMIT,
        .datetime     = UCM_BUILD_TIME,
        .target       = UCM_BUILD_TARGET,
        .compiler     = UCM_BUILD_CC,
        .options      = UCM_BUILD_OPTS,
        .flags        = UCM_BUILD_FLAGS,
    },
    .base.info.flags       = UCM_FLAG_PLUG_LOGGED,

    .base.info.pid         = L"ucm_core",
    .base.info.name        = L"UniChatMod core plugin",
    .base.info.developer   = L"SkyMaverick",
    .base.info.description = L"UniChatMod core library plugin",
    .base.info.copyright   = L"Zlib",
    .base.info.email       = L"",
    .base.info.website     = L"",

    .base.run         = _run_core,
    .base.stop        = _stop_core,
    .base.message     = _message_core
};

ucm_plugin_t* ucm_core = (ucm_plugin_t*) &core_lib;
