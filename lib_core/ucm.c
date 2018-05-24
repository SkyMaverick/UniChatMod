#include "ucm.h"
#include "api.h"
#include "plugmgr.h"
#include "core.h"

UCM_RET
ucm_core_start ()
{
    //TODO build plugin stack, init and start core plugin
    plugins_load_registry();
    return UCM_RET_SUCCESS;
}

const ucm_plugin_info_t*
ucm_core_info (void)
{
    extern ucm_plugin_t* lib_core;
    return libcore->info;
}

UCM_RET
ucm_core_stop (void)
{
    //TODO stop core plugin, cleanup and release plugin stack
    plugins_release_registry();
    return UCM_RET_SUCCESS;
}
