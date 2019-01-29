#include "osal.h"

inline int
osal_errno ()
{
    return p_error_get_last_system();
}
