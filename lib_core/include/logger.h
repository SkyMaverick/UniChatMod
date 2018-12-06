#ifndef _LOGGER_H_
#define _LOGGER_H_

#include "ucm.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

void log_init(void);
void log_release(void);

void logger_vlog(ucm_plugin_t* plugin, uint32_t type, const char* fmt, va_list va);
void logger_log(ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...);
void ucm_vlog(const char* fmt, va_list va);
void ucm_log(const char* fmt, ...);
void logger_connect(void (*callback)(ucm_plugin_t*,uint32_t,const char*));
void logger_disconnect(void (*callback)(ucm_plugin_t*,uint32_t,const char*));

#endif
