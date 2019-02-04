#include "osal-intrnl.h"

int
osal_errno ()
{
    return p_error_get_last_system();
}
