#ifndef _UCM_SYSTEM_ENTROPY_UPDATE_H_
#define _UCM_SYSTEM_ENTROPY_UPDATE_H_

#include "ucm.h"

UCM_RET
init_ucm_entropy (void);

int
get_ucm_entropy (void);

void
free_ucm_entropy (void);

#endif
