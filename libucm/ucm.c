#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>

#include "ucm.h"
#include "api.h"
#include "plugmgr.h"
#include "core.h"
#include "logger.h"
#include "defs.h"

LIBUCM_API const ucm_functions_t*
ucm_core_start (ucm_cargs_t* args)
{
//  TODO build plugin stack, init and start core plugin
    extern char ucm_path [UCM_PATH_MAX];
    extern char ucm_path_store[UCM_PATH_MAX];
    extern char ucm_path_plugs[UCM_PATH_MAX];

    if (args->path_abs && args->path_plug_abs && args->path_store_abs) {
        snprintf(ucm_path, UCM_PATH_MAX, "%s", args->path_abs);
        snprintf(ucm_path_store, UCM_PATH_MAX, "%s", args->path_store_abs);
        snprintf(ucm_path_plugs, UCM_PATH_MAX, "%s", args->path_plug_abs);
    } else {
        return NULL;
    }

    return (ucm_core->run() == UCM_RET_SUCCESS) ? UniAPI : NULL;
}

LIBUCM_API const ucm_plugin_info_t*
ucm_core_info (void)
{
    return &(ucm_core->info);
}

LIBUCM_API UCM_RET
ucm_core_stop (void)
{
    //TODO stop core plugin, cleanup and release plugin stack
    //TODO Move to core plugin functionality
    
    ucm_core->stop();
//
//    plugins_release_registry();
//
//
    return UCM_RET_SUCCESS;
}
