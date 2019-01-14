#include <stdlib.h>
#include <time.h>

#include "ucm.h"
#include "cpentupd.h"

UCM_RET
init_ucm_entropy (void)
{
    srand ((unsigned int) time(NULL));
    return UCM_RET_SUCCESS;
}

int
get_ucm_entropy (void)
{
    return rand();
}

void
free_ucm_entropy (void)
{
    // TODO
}
