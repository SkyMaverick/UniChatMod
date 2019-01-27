#pragma once

#include "osal-threading.c"
#include "osal-memory.c"
#include "osal-filesys.c"
#include "osal-dynlib.c"

void
osal_init (void);
void
osal_release (void);
