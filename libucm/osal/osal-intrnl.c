#include "osal.h"
#include "osal-intrnl.h"

static osal_handler_t osal_kernel = {
    .loop_system  = NULL,
    .loop_network = NULL
};
osal_handler_t* o_krnl = &osal_kernel;
