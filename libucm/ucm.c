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
    int ret_code = ucm_core->run();
    if ( ret_code != UCM_RET_SUCCESS) {
        ucm_etrace("%s\n", ucm_strerr (ret_code));
        return NULL;
    };

    return UniAPI;
}

LIBUCM_API const ucm_plugin_info_t*
ucm_core_info (void)
{
    return &(ucm_core->info);
}

LIBUCM_API UCM_RET
ucm_core_stop (void)
{
    ucm_core->stop();
    return UCM_RET_SUCCESS;
}
