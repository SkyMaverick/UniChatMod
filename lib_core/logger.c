#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include "ucm.h"
#include "config.h"
#include "logger.h"
#include "api.h"
#include "osal.h"

typedef struct _logger_s {
    void (*cb_log)(ucm_plugin_t* plug, uint32_t type, const char* text);
    struct _logger_s* next;
} ucm_logger_t;

static ucm_logger_t* logs;
static uintptr_t lock_mtx;
static uint32_t log_types = UCM_TYPE_LOG_INFO  |
                            UCM_TYPE_LOG_DEBUG |
                            UCM_TYPE_LOG_ERROR;

static void
_log_core (ucm_plugin_t* plug,
           uint32_t      type,
           const char* txt)
{
    switch (type){
        case UCM_TYPE_LOG_DEBUG:
                #ifdef DEBUG
                        fwrite(txt,strlen(txt),1,stdout);
                        break;
                #endif
                        return;
        case UCM_TYPE_LOG_INFO: fwrite(txt,strlen(txt),1,stdout);
                        break;
        case UCM_TYPE_LOG_ERROR: fwrite(txt,strlen(txt),1,stderr);
                        break;
    }

    ucm_rwlock_rlock(lock_mtx);
    for (ucm_logger_t* i=logs; i; i=i->next)
        i->cb_log(plug,type,txt);
    ucm_rwlock_unlock(lock_mtx);
}

static int
_log_enabled (ucm_plugin_t* plug,
              uint32_t      type)
{
    if(plug && ( !(plug->info.flags == UCM_FLAG_PLUG_LOGGED) ))
        return 0;
    if(plug && ( !(type & log_types) ))
        return 0;
    return 1;
}

static void
_log_flush (ucm_logger_t** list)
{
    ucm_logger_t* tmp = NULL;
    while((*list)){
        tmp = *list;
        (*list)=(*list)->next;
        ucm_free_null (tmp);
    }
}

void
log_init (void)
{
    logs = NULL;
    lock_mtx = ucm_rwlock_create();
}

void
log_release (void)
{
    _log_flush(&logs);
    ucm_rwlock_free(lock_mtx);
}

void
logger_vlog (ucm_plugin_t* plugin,
             uint32_t      type,
             const char*   fmt,
             va_list va)
{
    if(!_log_enabled(plugin,type))
        return;

    char buf[UCM_DEF_STRLEN];
    if(vsnprintf(buf,UCM_DEF_STRLEN,fmt,va))
        _log_core(plugin,type,buf);
}

void
logger_log (ucm_plugin_t* plugin,
            uint32_t      type,
            const char*   fmt,
            ...)
{
    if(!_log_enabled(plugin,type))
        return;

    va_list va;
    va_start(va, fmt);
    logger_vlog(plugin,type,fmt,va);
    va_end(va);
}

void
ucm_vlog (const char* fmt,
          va_list     va)
{
    char buf[UCM_DEF_STRLEN];
    if(vsnprintf(buf,UCM_DEF_STRLEN,fmt,va))
        _log_core(NULL,UCM_TYPE_LOG_INFO,buf);
}

void
ucm_log (const char* fmt,
         ...)
{
    va_list va;
    va_start(va, fmt);
    ucm_vlog(fmt,va);
    va_end(va);
}

void
logger_connect ( void (*callback)(ucm_plugin_t*,uint32_t,const char*) )
{
    ucm_logger_t* tmp = ucm_zmalloc (sizeof(ucm_logger_t));
    if (tmp) {
        ucm_rwlock_wlock(lock_mtx);
        tmp->cb_log = callback;
        tmp->next = logs;
        logs = tmp;
        ucm_rwlock_unlock(lock_mtx);
    }
}

void
logger_disconnect( void (*callback)(ucm_plugin_t*,uint32_t,const char*) )
{
    ucm_logger_t* prev = NULL;

    ucm_rwlock_wlock(lock_mtx);
    for(ucm_logger_t* i = logs; i;prev=i,i=i->next) {
        if(i->cb_log == callback){
            if(prev){
                prev->next = i->next;
            }else{
                logs = i->next;
            }
            ucm_free(i);
            break;
        }
    }
    ucm_rwlock_unlock(lock_mtx);
}
