#pragma once

#include "ucm.h"

#ifdef __cplusplus
extern "C" {
#endif

extern ucm_functions_t* UniAPI;

#define API UniAPI->app
#ifndef ONLY_WITH_CLIENT_API
    #define API_UV UniAPI->uv
#endif
#define API_OS UniAPI->sys

#if defined(DEBUG)
    #define ucm_dtrace(format, ...)                                                                \
        { UniAPI->app.log(ucm_core, UCM_TYPE_LOG_INFO, format, __VA_ARGS__); }
#else
    #define ucm_dtrace(format, ...)
#endif

#define ucm_trace(format, ...)                                                                     \
    { UniAPI->app.log(ucm_core, UCM_TYPE_LOG_INFO, format, __VA_ARGS__); }
#define ucm_etrace(format, ...)                                                                    \
    { UniAPI->app.log(ucm_core, UCM_TYPE_LOG_ERROR, format, __VA_ARGS__); }

#define ucm_strerr(code) UniAPI->sys.strerr(code)

#define ucm_free_null(X)                                                                           \
    do {                                                                                           \
        UniAPI->sys.free(X);                                                                       \
        X = NULL;                                                                                  \
    } while (0)

#ifdef __cplusplus
}
#endif
