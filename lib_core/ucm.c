#include "ucm.h"
#include "api.h"
#include "plugmgr.h"

UCM_RET
ucm_core_start ()
{
    //TODO build plugin stack, init and start core plugin
    plugins_load_registry();
    return UCM_RET_SUCCESS;
}

UCM_RET
ucm_core_send_message ()
{
    // TODO send message function
    return UCM_RET_CORE;
}

void
ucm_core_recv_register ()
{
    // TODO register recive message callback
}

UCM_RET
ucm_core_stop (void)
{
    //TODO stop core plugin, cleanup and release plugin stack
    plugins_release_registry();
    return UCM_RET_SUCCESS;
}
