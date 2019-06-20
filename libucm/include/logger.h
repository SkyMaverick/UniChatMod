#pragma once

#include "ucm.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

UCM_RET log_init(void);
void log_release(void);

void logger_vlog(ucm_plugin_t* plugin, uint32_t type, const char* fmt, va_list va);
void logger_log(ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...);
void ucm_vlog(const char* fmt, va_list va);
void ucm_log(const char* fmt, ...);
void logger_connect(void (*callback)(ucm_plugin_t*,uint32_t,const char*,void*), void* ctx);
void logger_disconnect(void (*callback)(ucm_plugin_t*,uint32_t,const char*,void*), void* ctx);
