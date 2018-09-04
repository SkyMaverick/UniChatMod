#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "ucm.h"
#include "alloc.h"
#include "network.h"
#include "net_legacy.h"

static void
__listen_func (/*TODO*/)
{

}

ucm_conptr_t
ucl_connect ()
{
    ucl_connection_t* con_s = ucm_zmalloc (sizeof(ucl_connection_t));
    if (con_s){
        
    }
    return (uintptr_t) con_s;
}

int
ucl_disconnect (ucm_conptr_t* cptr)
{
    if (cptr && *cptr > 0) {
        // TODO release connection
        ucm_free ((ucl_connection_t*)(*cptr));
        *cptr = 0;
    }
    return UCM_RET_SUCCESS;
}
