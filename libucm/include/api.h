#pragma once

#include "ucm.h"

#ifdef __cplusplus
    extern "C" {
#endif

uintptr_t
compat_layer_init (void);
void 
compat_layer_release (void);

extern ucm_functions_t* UniAPI;

#define ucm_free_null(X)        \
    do {                        \
        UniAPI->sys.free(X);    \
        X = NULL;               \
    } while (0)

#ifdef __cplusplus
    }
#endif
