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
#include "cpentupd.h"

typedef struct {
    // base plugin functionality (start/stop/mq)
    ucm_plugin_t        base;

    /* System-defines polling loop based on LibUV library
       and provide this for async fs, network etc. operations */
    uv_loop_t*          loop_system;    // idle loop (as default)
    uv_loop_t*          loop_network;   // different network loop

    // message/event communication loop (custom implementation)
    uintptr_t           loop_ucore;
} ucm_core_t;
// forward structure declaration
static ucm_core_t kernel;

static void*
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
    return NULL;
}

static UCM_RET
_stop_core (void)
{
    if (kernel.loop_ucore) {
        UniAPI->app.mainloop_msg_send(UCM_EVENT_TERM, (uintptr_t)ucm_core, 0, 0);
        UniAPI->sys.thread_join(kernel.loop_ucore);
        UniAPI->sys.thread_cleanup (&kernel.loop_ucore);
    }

    plugins_stop_all();

    plugins_release_registry();
    free_ucm_entropy();
    log_release();

    UniAPI->uv.loop_close (kernel.loop_system);
    ucm_free_null (kernel.loop_system);

    UniAPI->uv.loop_close (kernel.loop_network);
    ucm_free_null (kernel.loop_network);

//    if (kernel.loop_ucore > 0) {
//        UniAPI->app.mainloop_msg_send(UCM_EVENT_TERM, (uintptr_t)ucm_core, 0, 0);
//        UniAPI->sys.thread_join(kernel.loop_ucore);
//        UniAPI->sys.thread_cleanup (&kernel.loop_ucore);
//    }
//    db_close();
//    plugins_stop_all();
//    ucm_mloop_free();
//
//    hooks_event_release();

    return UCM_RET_SUCCESS;
}

static UCM_RET
_run_core (void)
{
    kernel.loop_system = UniAPI->sys.zmalloc (sizeof(uv_loop_t));
    if (kernel.loop_system == NULL)
        return UCM_RET_NONALLOC;
    kernel.loop_network = UniAPI->sys.zmalloc (sizeof(uv_loop_t));
    if (kernel.loop_network == NULL) {
        ucm_free_null (kernel.loop_system);
        return UCM_RET_NONALLOC;
    }

    uv_loop_init (kernel.loop_system);
    uv_loop_init (kernel.loop_network);

    log_init();
    init_ucm_entropy();
    
    plugins_load_registry(UniAPI->app.get_plugins_path());
    // TODO start uv_loop-s
    if ( ucm_mloop_init(UCM_DEF_MQ_LIMIT) == UCM_RET_SUCCESS ) {
        hooks_event_init ();
        kernel.loop_ucore = UniAPI->sys.thread_create(loop_core, NULL);
    }

    plugins_run_all ();

    /* Init subsystems */
//    extern wchar_t ucm_path_store [UCM_PATH_MAX];
//    char aPath [UCM_PATH_MAX];
//
//    if ( wcstombs(aPath, ucm_path_store, UCM_PATH_MAX) <= 0) {
//        ucm_etrace ("%s: %s\n", "Don't parse database file path", aPath);
//        return UCM_RET_WRONGPARAM;
//    }
//    // start main message loop
//    if ( ucm_mloop_init(UCM_DEF_MQ_LIMIT) == UCM_RET_SUCCESS ) {
//        hooks_event_init();
//        plugins_run_all();
//
//        if ( db_open(aPath, 0) != UCM_RET_SUCCESS) {
//            ucm_etrace ("%s: %s\n", "Couldn't open this database file", aPath);
//            _stop_core();
//            return UCM_RET_WRONGPARAM;
//        }
//
//        loop_ucore = UniAPI->sys.thread_create(loop_core, NULL);
//    }
//    ucm_dtrace("%s: %s\n", _("Success start UniChatMod core ver."), UCM_VERSION);
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

/***************************************************
    EXTERANAL FUNCTIONS
 ***************************************************/

uv_loop_t*
get_handle_mainloop (void)
{
    return kernel.loop_system;
}

uv_loop_t*
get_handle_netloop (void)
{
    return kernel.loop_network;
}

static ucm_core_t kernel = {
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

ucm_plugin_t* ucm_core = (ucm_plugin_t*) &kernel;
