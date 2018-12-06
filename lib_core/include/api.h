#ifndef _UCM_API_H_
#define _UCM_API_H_

#include "ucm.h"
#include "alloc.c"

#ifdef __cplusplus
    extern "C" {
#endif

extern ucm_functions_t* ucm_api;

#define ucm_kfree_null(X)   \
    do {                    \
        ucm_kfree(X);       \
        X = NULL;           \
    } while (0)

#ifdef __cplusplus
    }
#endif

#endif
