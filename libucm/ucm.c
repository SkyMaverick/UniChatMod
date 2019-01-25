#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>

#include "ucm.h"
#include "api.h"
#include "plugmgr.h"
#include "core.h"
#include "logger.h"
#include "defs.h"
#include "cpentupd.h"

#include <plibsys.h>

LIBUCM_API const ucm_functions_t*
ucm_core_start (ucm_cargs_t* args)
{
//  TODO build plugin stack, init and start core plugin
    extern wchar_t ucm_path [UCM_PATH_MAX];
    extern wchar_t ucm_path_store[UCM_PATH_MAX];
    extern wchar_t ucm_path_plugs[UCM_PATH_MAX];

    if (args->path_abs && args->path_plug_abs && args->path_store_abs) {
        if (!(     (mbstowcs (ucm_path      , args->path_abs      , UCM_PATH_MAX)) > 0
                && (mbstowcs (ucm_path_store, args->path_store_abs, UCM_PATH_MAX)) > 0
                && (mbstowcs (ucm_path_plugs, args->path_plug_abs , UCM_PATH_MAX)) > 0 ) )
            return NULL;
        p_libsys_init();
        log_init();
        init_ucm_entropy();

        plugins_load_registry (args->path_plug_abs);
        ucm_dtrace ("%s : %s\n", "Path in LIB", args->path_plug_abs);
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
    ucm_core->stop();

    plugins_release_registry();

    free_ucm_entropy();
    log_release();

    p_libsys_shutdown ();
    return UCM_RET_SUCCESS;
}
