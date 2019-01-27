#include "osal.h"

void
osal_init (void)
{
    p_libsys_init();
}

void
osal_release (void)
{
    p_libsys_shutdown();
}
