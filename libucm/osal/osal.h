#pragma once

#include <plibsys.h>
#include <stdint.h>
#include <stdbool.h>
#include <uv.h>
#include <assert.h>

#include "osal-dynlib.h"
#include "osal-filesys.h"
#include "osal-memory.h"
#include "osal-threading.h"
#include "osal-system.h"

void
osal_init (void);
void
osal_release (void);
