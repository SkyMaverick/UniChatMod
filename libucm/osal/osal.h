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

#if (defined(__GNUC__) && (__GNUC__ > 2 && __GNUC_MINOR__ > 0)) || \
    (defined(__INTEL_COMPILER) && __INTEL_COMPILER >= 800) || \
    __has_builtin(__builtin_expect)
#  define CC_LIKELY(x) __builtin_expect(!!(x), 1)
#  define CC_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#  define CC_LIKELY(x) (x)
#  define CC_UNLIKELY(x) (x)
#endif

void
osal_init (void);
void
osal_release (void);
