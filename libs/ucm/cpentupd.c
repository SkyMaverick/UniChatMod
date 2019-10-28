#include "cpentupd.h"

#include "ucm.h"

#include <stdlib.h>
#include <time.h>

UCM_RET
init_ucm_entropy(void) {
    srand((unsigned int)time(NULL));
    return UCM_RET_SUCCESS;
}

int
get_ucm_entropy(void) {
    return rand();
}

void
free_ucm_entropy(void) {
    // TODO
}
