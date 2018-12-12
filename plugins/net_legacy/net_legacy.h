#ifndef _UCM_PLUGIN_NET_LEGACY_H_
#define _UCM_PLUGIN_NET_LEGACY_H_

#include "ucm.h"

extern const ucm_functions_t* app;
extern const ucm_plugproto_t* plucl;

#define trace_dbg(fmt, ...) {app->log ( (ucm_plugin_t*)(plucl), UCM_TYPE_LOG_DEBUG, fmt, __VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log ( (ucm_plugin_t*)(plucl), UCM_TYPE_LOG_INFO,  fmt, __VA_ARGS__);}
#define trace_err(fmt, ...) {app->log ( (ucm_plugin_t*)(plucl), UCM_TYPE_LOG_ERROR, fmt, __VA_ARGS__);}

#define ucm_free_null(X)    \
    do {                    \
        app->free(X);       \
        X = NULL;           \
    } while (0)

#endif
