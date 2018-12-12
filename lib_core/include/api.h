#ifndef _UCM_API_H_
#define _UCM_API_H_

#include "ucm.h"
#include "osal.h"

#ifdef __cplusplus
    extern "C" {
#endif

extern ucm_functions_t* UniAPI;

#define ucm_free_null(X)    \
    do {                    \
        ucm_free(X);        \
        X = NULL;           \
    } while (0)

#ifdef __cplusplus
    }
#endif

#endif
