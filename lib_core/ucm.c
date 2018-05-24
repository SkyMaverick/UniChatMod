#include <stdlib.h>
#include <stdio.h>

#include "ucm.h"
#include "api.h"
#include "plugmgr.h"
#include "core.h"

UCM_RET
ucm_core_start (const char* path_abs,
                const char* path_store_abs)
{
    //TODO build plugin stack, init and start core plugin
    extern char ucm_path [UCM_PATH_MAX];
    extern char ucm_path_store[UCM_PATH_MAX];
    extern char ucm_path_plugs[UCM_PATH_MAX];

    if (path_abs) {
        snprintf (ucm_path, UCM_PATH_MAX, "%s", path_abs);
        snprintf (ucm_path_plugs, UCM_PATH_MAX, "%s/%s", path_abs, "mods");
    } else {
        ucm_dtrace("%s\n", "Library location path is empty");
        return UCM_RET_WRONGPARAM;
    }
    if (path_store_abs) {
        snprintf (ucm_path_store, UCM_PATH_MAX, "%s", path_store_abs);
    } else {
        ucm_dtrace("%s\n", "Store location path is empty");
        return UCM_RET_WRONGPARAM;
    }

    plugins_load_registry(ucm_path_plugs);
    lib_core->run();
    return UCM_RET_SUCCESS;
}

const ucm_plugin_info_t*
ucm_core_info (void)
{
    return lib_core->info;
}

UCM_RET
ucm_core_stop (void)
{
    //TODO stop core plugin, cleanup and release plugin stack
    lib_core->stop();
    plugins_release_registry();
    return UCM_RET_SUCCESS;
}
