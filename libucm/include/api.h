#pragma once

#include "ucm.h"

#ifdef __cplusplus
extern "C" {
#endif

uintptr_t compat_layer_init(void);
void compat_layer_release(void);

extern ucm_functions_t* UniAPI;

#if defined(DEBUG)
    #define ucm_dtrace(format, ...)                                            \
        {                                                                      \
            UniAPI->app.log(ucm_core, UCM_TYPE_LOG_INFO, format, __VA_ARGS__); \
        }
#else
    #define ucm_dtrace(format, ...)
#endif

#define ucm_trace(format, ...)                                             \
    {                                                                      \
        UniAPI->app.log(ucm_core, UCM_TYPE_LOG_INFO, format, __VA_ARGS__); \
    }
#define ucm_etrace(format, ...)                                             \
    {                                                                       \
        UniAPI->app.log(ucm_core, UCM_TYPE_LOG_ERROR, format, __VA_ARGS__); \
    }

#define ucm_strerr(code) UniAPI->sys.strerr(code)

#define ucm_free_null(X)     \
    do {                     \
        UniAPI->sys.free(X); \
        X = NULL;            \
    } while (0)

#ifdef __cplusplus
}
#endif
