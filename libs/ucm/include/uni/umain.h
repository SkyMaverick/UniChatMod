#pragma once

typedef uint8_t ucm_object_t;

#define OBJECT(X) ((ucm_object_t)(X))

typedef char ucm_chr_t;
typedef ucm_chr_t* ucm_str_t;
typedef uint32_t ucm_wchr_t;
typedef ucm_wchr_t* ucm_wstr_t;

#define UCMSZ_CHR sizeof(ucm_chr_t)
#define UCMSZ_WCHR sizeof(ucm_wchr_t)

enum { UCM_TYPE_LOG_INFO = 1 << 0, UCM_TYPE_LOG_DEBUG = 1 << 1, UCM_TYPE_LOG_ERROR = 1 << 2 };

enum {
    UCM_TYPE_OBJECT_NULL = 0,
    UCM_TYPE_OBJECT_PLUGIN = 1 << 0,
    UCM_TYPE_OBJECT_EVENT = 1 << 1,
    UCM_TYPE_OBJECT_CONTACT = 1 << 2,
    UCM_TYPE_OBJECT_CONNECT = 1 << 3
};

// Global flags ---------------
enum {
    UCM_FLAG_TERMINATE = 0,
    UCM_FLAG_PROFILE_RO = 1 << 0,
    UCM_FLAG_PROFILE_NEW = 1 << 1,
    UCM_FLAG_PROFILE_CHECK = 1 << 2
};

enum { CORE_LOOP_MAIN = 1, CORE_LOOP_SYSTEM = 2, CORE_LOOP_NETWORK = 3 };
