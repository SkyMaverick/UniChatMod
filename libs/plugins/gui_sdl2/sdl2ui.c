#include "sdl2ui.h"

#include "config.h"
#include "ucm.h"

const ucm_functions_t* app;

static uintptr_t hwnd = 0;
static int term = 1;

// *********************************************************
//      PLUGIN FUNCTIONS
// *********************************************************

static UCM_RET
_run_plugin(void) {
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_plugin(void) {
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2) {
    switch (id) {
    case UCM_SIG_START_GUI: {
        break;
    }
    case UCM_SIG_TERM: {
        break;
    }
    };
    UNUSED(x1);
    UNUSED(x2);
}

static ucm_plugui_t plugin = {
    .core.oid = UCM_TYPE_OBJECT_PLUGIN,
    .core.info.api = {.vmajor = UCM_API_MAJOR_VER, .vminor = UCM_API_MINOR_VER},
    .core.info.sys = UCM_TYPE_PLUG_GUI,
    .core.info.vmajor = UCM_VERSION_MAJOR,
    .core.info.vminor = UCM_VERSION_MINOR,
    .core.info.vpatch = UCM_VERSION_PATCH,
    .core.info.flags = 0,
    .core.info.build =
        {
            .commit = UCM_BUILD_COMMIT,
            .datetime = UCM_BUILD_TIME,
            .target = UCM_BUILD_TARGET,
            .compiler = UCM_BUILD_CC,
            .options = UCM_BUILD_OPTS,
            .flags = UCM_BUILD_FLAGS,
        },
    .core.info.pid = "ea1f4f27-bdfc-451f-a83c-9e0b80753020",
    .core.info.name = L"std_uisdl2",
    .core.info.developer = L"SkyMaverick",
    .core.info.description = L"Interface plugin based on SDL2 library",
    .core.info.copyright = L"Zlib",
    .core.info.email = L"mail@mail.ru",
    .core.info.website = L"http://null.org",

    .core.run = _run_plugin,
    .core.stop = _stop_plugin,
    .core.message = _message,
};

const ucm_plugui_t* plug = &plugin;

LIBUCM_API ucm_plugin_t*
_init_plugin(const ucm_functions_t* api) {
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
