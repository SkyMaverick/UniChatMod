#include "logger.h"

#include "api.h"
#include "config.h"
#include "ucm.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct _logger_s {
    void (*cb_log)(ucm_plugin_t* plug, uint32_t type, const char* text, void* ctx);
    void* ctx;
    struct _logger_s* next;
} ucm_logger_t;

static ucm_logger_t* logs = NULL;
static uintptr_t lock_mtx;
static uint32_t log_types = UCM_TYPE_LOG_INFO | UCM_TYPE_LOG_DEBUG | UCM_TYPE_LOG_ERROR;

#define LOG_BUFFER_SIZE 65535
#define LOG_TYPE_SIZE sizeof(uint32_t)

static char* buffer = NULL;
static char* buffer_tmp = NULL;

static void
_log_console(const char* txt, uint32_t type) {
    switch (type) {
    case UCM_TYPE_LOG_DEBUG:
#ifdef DEBUG
        fwrite(txt, strlen(txt), 1, stdout);
        break;
#endif
        return;
    case UCM_TYPE_LOG_INFO:
        fwrite(txt, strlen(txt), 1, stdout);
        break;
    case UCM_TYPE_LOG_ERROR:
        fwrite(txt, strlen(txt), 1, stderr);
        break;
    }
}

static void
_log_core(ucm_plugin_t* plug, uint32_t type, const char* txt) {
    if (lock_mtx) {
        UniAPI->sys.mutex_lock(lock_mtx);
#ifndef ENABLE_CURSES_UI
        _log_console(txt, type);
#endif
        for (ucm_logger_t* i = logs; i; i = i->next)
            i->cb_log(plug, type, txt, i->ctx);

#ifdef DEBUG
        if (buffer) {
#else
        if (buffer && (type != UCM_TYPE_LOG_DEBUG)) {
#endif
            size_t len = strlen(txt);
            if ((buffer_tmp - buffer + len + 1 + LOG_TYPE_SIZE) < LOG_BUFFER_SIZE) {
                memcpy(buffer_tmp + LOG_TYPE_SIZE, txt, len);
                *buffer_tmp = type;
                buffer_tmp += len + LOG_TYPE_SIZE;
                *(++buffer_tmp) = '\0';
            }
        }

        UniAPI->sys.mutex_unlock(lock_mtx);
    }
}

static int
_log_enabled(ucm_plugin_t* plug, uint32_t type) {
    if (plug && (!(plug->info.flags == UCM_FLAG_PLUG_LOGGED)))
        return 0;
    if (plug && (!(type & log_types)))
        return 0;
    return 1;
}

static void
_buffer_release(void) {
    if (buffer) {
        ucm_free_null(buffer);
        buffer_tmp = NULL;
    }
}

static void
_log_flush(ucm_logger_t** list) {
    ucm_logger_t* tmp = NULL;
    while ((*list)) {
        tmp = *list;
        (*list) = (*list)->next;
        ucm_free_null(tmp);
    }
}

UCM_RET
log_init(void) {
    lock_mtx = UniAPI->sys.mutex_create();
    if (!lock_mtx) {
        return UCM_RET_SYSTEM_NOCREATE;
    }
    buffer = UniAPI->sys.zmalloc(LOG_BUFFER_SIZE);
    if (!buffer) {
        return UCM_RET_SYSTEM_NOMEMORY;
    }
    buffer_tmp = buffer;
    return UCM_RET_SUCCESS;
}

void
log_release(void) {
    while (UniAPI->sys.mutex_trylock(lock_mtx) != 0) {
        //        fprintf (stdout, "LOCKTRY WRITE MUTEX\n");
        continue;
    }

    _log_flush(&logs);
    _buffer_release();

    UniAPI->sys.mutex_unlock(lock_mtx);
    UniAPI->sys.mutex_free(lock_mtx);
}

void
logger_vlog(ucm_plugin_t* plugin, uint32_t type, const char* fmt, va_list va) {
    if (!_log_enabled(plugin, type))
        return;

    char buf[UCM_DEF_STRLEN];
    if (vsnprintf(buf, UCM_DEF_STRLEN, fmt, va))
        _log_core(plugin, type, buf);
}

void
logger_log(ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...) {
    if (!_log_enabled(plugin, type))
        return;

    va_list va;
    va_start(va, fmt);
    logger_vlog(plugin, type, fmt, va);
    va_end(va);
}

void
ucm_vlog(const char* fmt, va_list va) {
    char buf[UCM_DEF_STRLEN];
    if (vsnprintf(buf, UCM_DEF_STRLEN, fmt, va))
        _log_core(NULL, UCM_TYPE_LOG_INFO, buf);
}

void
ucm_log(const char* fmt, ...) {
    va_list va;
    va_start(va, fmt);
    ucm_vlog(fmt, va);
    va_end(va);
}

void
logger_connect(void (*callback)(ucm_plugin_t*, uint32_t, const char*, void*), void* ctx) {
    ucm_logger_t* tmp = UniAPI->sys.zmalloc(sizeof(ucm_logger_t));
    if (tmp) {
        while (UniAPI->sys.mutex_trylock(lock_mtx) != 0) {
            continue;
        }

        tmp->cb_log = callback;
        tmp->next = logs;
        tmp->ctx = ctx;
        logs = tmp;

        // Flush buffer in first connected logger
        if (buffer) {
            char* mark = buffer;
            while (mark != buffer_tmp) {
                callback(NULL, *((uint32_t*)mark), mark + LOG_TYPE_SIZE, ctx);
                mark += strlen(mark + LOG_TYPE_SIZE) + LOG_TYPE_SIZE + 1;
            }
            _buffer_release();
        }

        UniAPI->sys.mutex_unlock(lock_mtx);
    }
}

void
logger_disconnect(void (*callback)(ucm_plugin_t*, uint32_t, const char*, void*)) {
    ucm_logger_t* prev = NULL;

    while (UniAPI->sys.mutex_trylock(lock_mtx) != 0) {
        continue;
    }

    for (ucm_logger_t* i = logs; i; prev = i, i = i->next) {
        if (i->cb_log == callback) {
            if (prev) {
                prev->next = i->next;
            } else {
                logs = i->next;
            }
            UniAPI->sys.free(i);
            break;
        }
    }
    UniAPI->sys.mutex_unlock(lock_mtx);
}
