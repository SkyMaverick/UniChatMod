#ifndef _UCM_PLUGIN_NET_LEGACY_H_
#define _UCM_PLUGIN_NET_LEGACY_H_

#include "ucm.h"

extern const ucm_functions_t* app;
extern const ucm_pclplugin_t* plucl;

#define trace_dbg(fmt, ...) {app->log ( (ucm_plugin_t*)(plucl), UCM_TYPE_LOG_DEBUG, fmt, __VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log ( (ucm_plugin_t*)(plucl), UCM_TYPE_LOG_INFO,  fmt, __VA_ARGS__);}
#define trace_err(fmt, ...) {app->log ( (ucm_plugin_t*)(plucl), UCM_TYPE_LOG_ERROR, fmt, __VA_ARGS__);}

#endif
