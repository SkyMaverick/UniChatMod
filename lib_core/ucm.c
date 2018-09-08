#include <stdlib.h>
#include <stdio.h>

#include "ucm.h"
#include "api.h"
#include "plugmgr.h"
#include "core.h"
#include "defs.h"

const ucm_functions_t*
ucm_core_start (const char* path_abs,
                const char* path_plug_abs,
                const char* path_store_abs)
{
    //TODO build plugin stack, init and start core plugin
    extern wchar_t ucm_path [UCM_PATH_MAX];
    extern wchar_t ucm_path_store[UCM_PATH_MAX];
    extern wchar_t ucm_path_plugs[UCM_PATH_MAX];

    if (path_abs && path_plug_abs && path_store_abs) {
        if (!(     (mbstowcs (ucm_path      , path_abs      , UCM_PATH_MAX)) > 0
                && (mbstowcs (ucm_path_store, path_plug_abs , UCM_PATH_MAX)) > 0
                && (mbstowcs (ucm_path_plugs, path_store_abs, UCM_PATH_MAX)) > 0 ) )
            return NULL;
    }

    plugins_load_registry(ucm_path_plugs);
    ucm_core->run();
    return ucm_api;
}

const ucm_plugin_info_t*
ucm_core_info (void)
{
    return &(ucm_core->info);
}

UCM_RET
ucm_core_stop (void)
{
    //TODO stop core plugin, cleanup and release plugin stack
    ucm_core->stop();
    plugins_release_registry();
    return UCM_RET_SUCCESS;
}
