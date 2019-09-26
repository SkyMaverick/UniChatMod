#pragma once

typedef uint8_t ucm_object_t;

#define OBJECT(X) ((ucm_object_t)(X))

typedef char ucm_chr_t;
typedef ucm_chr_t* ucm_str_t;
typedef uint32_t ucm_wchr_t;
typedef ucm_wchr_t* ucm_wstr_t;

#define UCMSZ_CHR sizeof(ucm_chr_t)
#define UCMSZ_WCHR sizeof(ucm_wchr_t)

enum
{
    UCM_TYPE_LOG_INFO  = 1 << 0,
    UCM_TYPE_LOG_DEBUG = 1 << 1,
    UCM_TYPE_LOG_ERROR = 1 << 2
};

enum
{
    UCM_TYPE_OBJECT_NULL    = 0,
    UCM_TYPE_OBJECT_PLUGIN  = 1 << 0,
    UCM_TYPE_OBJECT_EVENT   = 1 << 1,
    UCM_TYPE_OBJECT_CONTACT = 1 << 2,
    UCM_TYPE_OBJECT_CONNECT = 1 << 3
};

// Global flags ---------------
enum
{
    UCM_FLAG_TERMINATE = 0,
    UCM_FLAG_ROPROF    = 60,
    UCM_FLAG_NEWPROF   = 61,
    UCM_FLAG_CHECKPROF = 62
};

enum
{
    CORE_LOOP_MAIN    = 1,
    CORE_LOOP_SYSTEM  = 2,
    CORE_LOOP_NETWORK = 3
};

enum
{
    UCM_FLAG_MODE_NEWPROFILE = 1 << 0,
    UCM_FLAG_MODE_READONLY   = 1 << 1,
    UCM_FLAG_MODE_HEADLESS   = 1 << 2,
    UCM_FLAG_MODE_DAEMON     = 1 << 3
};

/*! Core return status */
typedef enum _ucm_status_return_e
{
    UCM_RET_SUCCESS = 0,
    UCM_RET_WRONGPARAM,
    UCM_RET_EXCEPTION,
    UCM_RET_NOTIMPLEMENT,
    UCM_RET_NOOBJECT,
    UCM_RET_EMPTY,
    UCM_RET_BUSY,

    UCM_RET_DATABASE_UNKWNERROR,
    UCM_RET_DATABASE_BADFORMAT,
    UCM_RET_DATABASE_BADINIT,
    UCM_RET_DATABASE_BADINTERNAL,
    UCM_RET_DATABASE_BADVERSION,
    UCM_RET_DATABASE_BADMETADATA,
    UCM_RET_DATABASE_NOTCHANGE,

    UCM_RET_SYSTEM_NOCREATE,
    UCM_RET_SYSTEM_NOACCESS,
    UCM_RET_SYSTEM_NOMEMORY,
    UCM_RET_SYSTEM_DLERROR,

    UCM_RET_MQUEUE_OVERFLOW,
    UCM_RET_MQUEUE_EMPTY,

    UCM_RET_PLUGIN_BADMODULE,
    UCM_RET_PLUGIN_BADVERSION,
    UCM_RET_PLUGIN_BADSYSTEM,
    UCM_RET_PLUGIN_BADPID,
    UCM_RET_PLUGIN_BADIFACE,

    UCM_RET_UNKNOWERROR
} UCM_RET;
