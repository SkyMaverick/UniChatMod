#include "core.h"

#include "api.h"
#include "config.h"
#include "cpentupd.h"
#include "db.h"
#include "defs.h"
#include "evhook.h"
#include "logger.h"
#include "mainloop.h"
#include "plugmgr.h"
#include "flags.h"
#include "ucm.h"

#include <wchar.h>

typedef struct {
    // base plugin functionality (start/stop/mq)
    ucm_plugin_t base;

    // message/event communication loop (custom implementation)
    uintptr_t loop_ucore;
    // osal system handle
    struct {
        uv_loop_t* loop_sys;
        uv_loop_t* loop_net;
    } uv;
} ucm_core_t;
// forward structure declaration
static ucm_core_t kernel;

static void*
loop_core(void* ctx) {
    uint32_t id;
    uintptr_t lctx;
    uint32_t x1;
    uint32_t x2;

    UNUSED(ctx);

    while (1) {
        while (ucm_mloop_pop(&id, &lctx, &x1, &x2) == UCM_RET_SUCCESS) {
            // hook events for host applications
            hooks_event(id, lctx, x1, x2);
            // send message to all plugins
            pmgr_message_process(id, lctx, x1, x2);

            switch (id) {
            case UCM_SIG_TERM:
                set_system_flag(UCM_FLAG_TERMINATE);
                ucm_dtrace("[EVENT] %s\n", "Catch TERM message. Core loop exit.");
                break;
            case UCM_SIG_PLUGS_SUCCESS:
                ucm_dtrace("[EVENT] %s: %d\n", "Found plugins", x1);
                db_open(UniAPI->app.get_store_path());
                break;
            }
            // free events context memory
            if (SIGNAL_ALLOCATED(id)) {
                ucm_dtrace("%s: %d\n", "Event destroy", id);
                UniAPI->app.mainloop_sig_free((ucm_signal_t**)(&lctx));
            }
        }
        if (get_system_flag(UCM_FLAG_TERMINATE)) {
            return NULL;
        }
        ucm_mloop_wait();
    }
    return NULL;
}

static UCM_RET
_stop_core(void) {
    return UCM_RET_SUCCESS;
}

static UCM_RET
_run_core(void) {
    ucm_dtrace("%s: %s\n", _("Success start UniChatMod core ver."), UCM_VERSION);
    return UCM_RET_SUCCESS;
}

static void
_message_core(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2)

{
    switch (id) {
    case UCM_SIG_DBLOAD_SUCCESS:
        if (x1 == UCM_RET_SUCCESS) {
            ucm_dtrace("[EVENT] %s: %S\n", "Start database with plugin",
                       ((ucm_plugin_t*)(ctx))->info.name);
            UniAPI->app.mainloop_msg_send(UCM_SIG_LOAD_SUCCESS, 0, 0, 0);
        }
        break;
    };
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

void
wait_core_loop(void) {
    UniAPI->sys.thread_join(kernel.loop_ucore);
}

static inline UCM_RET
uv_init(void) {
    kernel.uv.loop_sys = uv_default_loop();
    if (kernel.uv.loop_sys == NULL)
        return UCM_RET_NOOBJECT;
    kernel.uv.loop_net = UniAPI->sys.zmalloc(sizeof(uv_loop_t));
    if (kernel.uv.loop_net == NULL) {
        return UCM_RET_SYSTEM_NOMEMORY;
    }
    uv_loop_init(kernel.uv.loop_net);
    uv_run(kernel.uv.loop_sys, UV_RUN_DEFAULT);
    uv_run(kernel.uv.loop_net, UV_RUN_DEFAULT);

    return UCM_RET_SUCCESS;
}
static inline void
uv_destroy(void) {
    uv_loop_close(kernel.uv.loop_sys);
    uv_loop_close(kernel.uv.loop_net);
    if (kernel.uv.loop_net != NULL)
        ucm_free_null(kernel.uv.loop_net);
}

UCM_RET
core_load(void) {
    log_init();
    init_ucm_entropy();

    int ret_code = uv_init();
    if (ret_code == UCM_RET_SUCCESS) {
        ret_code = ucm_mloop_init(UCM_DEF_MQ_LIMIT);
        if (ret_code == UCM_RET_SUCCESS) {
            hooks_event_init();
            kernel.loop_ucore = UniAPI->sys.thread_create(loop_core, NULL);
        } else {
            uv_destroy();
            return ret_code;
        }

        pmgr_load(UniAPI->app.get_plugins_path());
    } else {
        uv_destroy();
    }
    return ret_code;
}

UCM_RET
core_unload(void) {
    if (kernel.loop_ucore) {
        UniAPI->app.mainloop_msg_send(UCM_SIG_TERM, (uintptr_t)ucm_core, 0, 0);
        UniAPI->sys.thread_join(kernel.loop_ucore);
        UniAPI->sys.thread_cleanup(&kernel.loop_ucore);
    }

    db_close();
    pmgr_unload();
    free_ucm_entropy();
    hooks_event_release();
    ucm_mloop_free();
    log_release();
    uv_destroy();

    return UCM_RET_SUCCESS;
}
/***************************************************
    EXTERANAL FUNCTIONS
 ***************************************************/

uintptr_t
get_loop_handle(int loop) {
    switch (loop) {
    case CORE_LOOP_MAIN:
        return kernel.loop_ucore;
    case CORE_LOOP_SYSTEM:
        return (uintptr_t)kernel.uv.loop_sys;
    case CORE_LOOP_NETWORK:
        return (uintptr_t)kernel.uv.loop_net;
    default:
        return 0;
    }
}

static ucm_core_t kernel = {.base.head.oid = UCM_TYPE_OBJECT_PLUGIN,
                            .base.info.api.vmajor = UCM_API_MAJOR_VER,
                            .base.info.api.vminor = UCM_API_MINOR_VER,
                            .base.info.sys = 0,
                            .base.info.vmajor = UCM_VERSION_MAJOR, // TODO
                            .base.info.vminor = UCM_VERSION_MINOR,
                            .base.info.vpatch = UCM_VERSION_PATCH,
                            .base.info.build =
                                {
                                    .commit = UCM_BUILD_COMMIT,
                                    .datetime = UCM_BUILD_TIME,
                                    .target = UCM_BUILD_TARGET,
                                    .compiler = UCM_BUILD_CC,
                                    .options = UCM_BUILD_OPTS,
                                    .flags = UCM_BUILD_FLAGS,
                                },
                            .base.info.flags = UCM_FLAG_PLUG_LOGGED,

                            .base.info.pid = "00000000-0000-0000-0000-000000000000",
                            .base.info.name = L"std_core",
                            .base.info.developer = L"SkyMaverick",
                            .base.info.description = L"UniChatMod core library plugin",
                            .base.info.copyright = L"Zlib",
                            .base.info.email = L"",
                            .base.info.website = L"",

                            .base.run = _run_core,
                            .base.stop = _stop_core,
                            .base.message = _message_core};

ucm_plugin_t* ucm_core = (ucm_plugin_t*)&kernel;
