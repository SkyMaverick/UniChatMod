#pragma once

#include <stdio.h>
#include <limits.h>

#include "ucm.h"
#include "config.h"
#include "gettext.h"
#include "api.h"
#include "core.h"

#if defined (ENABLE_DEBUG)
    #define ucm_dtrace(format,...) {UniAPI->app.log(ucm_core,UCM_TYPE_LOG_INFO,format,__VA_ARGS__); }
#else
    #define ucm_dtrace(format, ...)
#endif

#define ucm_trace(format,...) { UniAPI->app.log(ucm_core,UCM_TYPE_LOG_INFO,format,__VA_ARGS__);}
#define ucm_etrace(format,...) { UniAPI->app.log(ucm_core,UCM_TYPE_LOG_ERROR,format,__VA_ARGS__);}

#ifndef _countof
    #define _countof(x) (sizeof(x)/sizeof(x[0]))
#endif
