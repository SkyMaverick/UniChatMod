#pragma once

#include "ucm.h"

UCM_RET
init_ucm_entropy(void);

int
get_ucm_entropy(void);

void
free_ucm_entropy(void);
