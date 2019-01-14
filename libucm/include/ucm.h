#pragma once

/* GhostCommander (gc) - simple, modular, scalable file manager */

#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>
#include <limits.h>
#include <wchar.h>

#ifdef __cplusplus
    extern "C" {
#endif

#define ENABLE_WARN_DEPRECATED 1

#define UCM_API_MAJOR_VER 1
#define UCM_API_MINOR_VER 1

#if defined(__clang__)
    #define UCM_FUNC_DEPRECATED(x) __attribute__ ((deprecated(x)))
#elif defined(__GNUC__)
    #if !defined(__GNUC_PREREQ)
        #if defined(__GNUC_MINOR__)
            #define __GNUC_PREREQ(maj, min) \
                ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
        #else
            #define __GNUC_PREREQ(maj, min) 0
        #endif
    #endif

    #if __GNUC_PREREQ(4,5)
        #define UCM_FUNC_DEPRECATED(x) __attribute__ ((deprecated(x)))
    #else
        #define UCM_FUNC_DEPRECATED(x) __attribute__ ((deprecated))
    #endif
#else
    #define UCM_FUNC_DEPRECATED(x)
#endif

#ifndef UCM_API_LEVEL
    #define UCM_API_LEVEL UCM_API_MINOR_VER
#endif

#if (ENABLE_WARN_DEPRECATED && UCM_API_LEVEL >= 1)
    #define DEPRECATED_01 UCM_FUNC_DEPRECATED("This function deprecated in API ver. >= 0.1")
#else
    #define DEPRECATED_01
#endif

// === IMPORT / EXPORT ====================

#ifndef __has_attribute
    #define __has_attribute(x) (0)
#endif

#ifndef __ucm_export
    #if defined(_WIN32) || defined(__CYGWIN__)
        #if defined(__GNUC__) || __has_attribute(dllexport)
            #define __ucm_export __attribute__((dllexport))
        #elif defined(_MSC_VER)
            #define __ucm_export __declspec(dllexport)
        #else
            #define __ucm_export
        #endif
    #elif defined(__GNUC__) || __has_attribute(visibility)
        #define __ucm_export __attribute__((visibility("default")))
    #else
        #define __ucm_export
    #endif
#endif /* __ucm_export */

#ifndef __ucm_import
    #if defined(_WIN32) || defined(__CYGWIN__)
        #if defined(__GNUC__) || __has_attribute(dllimport)
            #define __ucm_import __attribute__((dllimport))
        #elif defined(_MSC_VER)
            #define __ucm_import __declspec(dllimport)
        #else
            #define __ucm_import
        #endif
    #else
        #define __ucm_import
    #endif
#endif /* __ucm_import */

#if defined(LIBUCM_EXPORTS)
    #define LIBUCM_API __ucm_export
#elif defined(LIBMDBX_IMPORTS)
    #define LIBUCM_API __ucm_import
#else
    #define LIBUCM_API
#endif

// === CONSTANTS ========================

#if defined(PATH_MAX)
    #define UCM_PATH_MAX PATH_MAX
#elif defined(_UCM_CONFIG_H_)
    #define UCM_PATH_MAX DEF_PATH_MAX
#else
    #define UCM_PATH_MAX 4096
#endif

#if defined(NAME_MAX)
    #define UCM_NAME_MAX NAME_MAX
#elif defined(_UCM_CONFIG_H_)
    #define UCM_NAME_MAX DEF_NAME_MAX
#else
    #define UCM_NAME_MAX 256
#endif

#define UCM_PID_MAX 100 * sizeof(wchar_t)
#define UNUSED(x) (void)(x);

#define UCM_CONTACT_ID_MAX   256
#define UCM_CONTACT_NAME_MAX 1024

#define UCM_DB_DEFAULT_NAME "ucmdb"

// *********************************************************
//      CORE STRUCTURES
// *********************************************************

typedef uint8_t ucm_object_t;

// TODO check uchar.h enabled
#if 1
    #include <uchar.h>
    typedef char      u8char_t;
    typedef char16_t  u16char_t;
    typedef char32_t  u32char_t;
#else
    typedef uint8_t   u8char_t;
    typedef uint16_t  u16char_t;
    typedef uint32_t  u32char_t;
#endif
#define U8CHAR_SIZE  sizeof(u8char_t)
#define U16CHAR_SIZE sizeof(u16char_t)
#define U32CHAR_SIZE sizeof(u32char_t)

/*! Core return status */
typedef enum _ucm_status_return_e {
    UCM_RET_SUCCESS,
    UCM_RET_UNKNOWERROR,
    UCM_RET_WRONGPARAM,
    UCM_RET_EXCEPTION,
    UCM_RET_UBOUND,
    UCM_RET_UNREALIZED,
    UCM_RET_NONALLOC,
    UCM_RET_NOOBJECT,
    UCM_RET_NOACCESS,
    UCM_RET_OVERFLOW,
    UCM_RET_EMPTY,
    UCM_RET_DUPLICATE,
    UCM_RET_BUSY,
    UCM_RET_INVALID,
    UCM_RET_BADVERSION
} UCM_RET;

enum {
    UCM_TYPE_LOG_INFO  = 1 << 0,
    UCM_TYPE_LOG_DEBUG = 1 << 1,
    UCM_TYPE_LOG_ERROR = 1 << 2
};

enum {
    UCM_TYPE_OBJECT_NULL    = 0,
    UCM_TYPE_OBJECT_PLUGIN  = 1 << 0,
    UCM_TYPE_OBJECT_EVENT   = 1 << 1,
    UCM_TYPE_OBJECT_CONTACT = 1 << 2,
    UCM_TYPE_OBJECT_MESSAGE = 1 << 3,
    UCM_TYPE_OBJECT_CONNECT = 1 << 4
};

// *********************************************************
//      BASE STRUCTURES
// *********************************************************

// Events ---------------------
enum {
    UCM_EVENT_TERM              = 0,
    UCM_EVENT_RUN               = 1 << 0,
    UCM_EVENT_INFO              = 1 << 1,
    UCM_EVENT_LOAD_SUCCESS      = 1 << 2,
    UCM_EVENT_START_GUI         = 1 << 3,
    UCM_EVENT_START_GUI2        = 1 << 4
};

typedef struct {
    ucm_object_t oid;

    uint8_t ev;
    size_t  size;
    void*   sender;
} ucm_ev_t;

typedef struct {
    ucm_ev_t base;
    // TODO
} ucm_evgui_t;

// Messages -------------------
enum {
    UCM_FLAG_MSG_MULTYCAST  = 1 << 0,
    UCM_FLAG_MSG_CRYPTO     = 1 << 1,
    UCM_FLAG_MSG_ALERT      = 1 << 2
};

enum {
    UCM_TYPE_MSG_NORMAL  = 0,
    UCM_TYPE_MSG_FILE    = 1,
    UCM_TYPE_MSG_ROOM    = 2,
    UCM_TYPE_MSG_STATUS  = 3,
    UCM_TYPE_MSG_EVENT   = 4
};

typedef struct ucm_msg_s {
    ucm_object_t oid;

    uint8_t     type;
    // TODO sender info
    time_t      time;
    uint32_t    flags;
    struct {
        size_t  size;
        uint8_t blob [1];
    } data;
} ucm_message_t;

typedef uint64_t HCONTACT;

typedef struct ucm_cont_s {
    ucm_object_t oid;            // ucm system object ID

    HCONTACT     cid;            // global contact ID
    struct {
        char* name;
    }  info;
    // TODO
} ucm_contact_t;

// Users ----------------------
enum {
    UCM_USER_STATUS_INACTIVE    = 0,
    UCM_USER_STATUS_ACTIVE      = 1 << 0,
    UCM_USER_STATUS_ABSENT      = 1 << 1,
    UCM_USER_STATUS_NAVIALABLE  = 1 << 2,
    UCM_USER_STATUS_BUSY        = 1 << 3,
    UCM_USER_STATUS_DNTDISTURB  = 1 << 4,
    UCM_USER_STATUS_MAYTALK     = 1 << 5,
    UCM_USER_STATUS_INVISIBLE   = 1 << 6,
};

// *********************************************************
//      PLUGINS FUNCTIONALITY API IMPLEMENTATION
// *********************************************************


#define _EVENT_SENDER(obj)\
    ((ucm_plugin_t*)(((ucm_ev_t*)obj)->sender))

/*! Enums what defines the plugin area */
enum {
    UCM_TYPE_PLUG_DB       = 1 << 0,
    UCM_TYPE_PLUG_PROTO    = 1 << 1,
    UCM_TYPE_PLUG_CRYPTO   = 1 << 2,
    UCM_TYPE_PLUG_HIST     = 1 << 3,
    UCM_TYPE_PLUG_GUI      = 1 << 4,
    UCM_TYPE_PLUG_STUFF    = 1 << 5
};

/*! Usage API version */
typedef struct {
    const uint8_t vmajor;
    const uint8_t vminor;
} ucm_vapi_t;

enum {
    UCM_FLAG_PLUG_LOGGED = 1,
};

typedef struct {
    const ucm_vapi_t     api;                /// plugin release api version (required)
    const uint8_t        sys;                /// plugin subsystem (required)
    const uint16_t       vmajor;             /// major plugin version (required)
    const uint16_t       vminor;             /// minor plugin version (required)
    const uint16_t       vpatch;             /// patch plugin version (required)
    uint32_t             flags;              /// plugin flags
    // build info.
    struct {
        const wchar_t*   commit;             /// commit in repository
        const wchar_t*   datetime;           /// build datetime
        const wchar_t*   target;             /// build target platform
        const wchar_t*   compiler;           /// build this compiler
        const wchar_t*   options;            /// build options
        const wchar_t*   flags;              /// build with flags
    } build;
    const wchar_t* const pid;                /// plugin id for internal ident (required)
    // developer info
    const wchar_t* const name;               /// plugin name for user
    const wchar_t* const developer;          /// developer name
    const wchar_t* const description;        /// plugin description and more info
    const wchar_t* const copyright;          /// plugin license
    const wchar_t* const email;              /// support email
    const wchar_t* const website;            /// official website
} ucm_plugin_info_t;

/*! Structure what defines base plugin interface*/
typedef struct _ucm_plugin_s {
    ucm_object_t      oid;                                      /// ucm system object identificator

    ucm_plugin_info_t info;
    UCM_RET           (*run)(void);                             /// activate plugin (with context for hot-plug) (required)
    UCM_RET           (*stop)(void);                            /// deactivate plugin (required)
    void              (*message)(uint32_t id, uintptr_t ctx,
                                 uint32_t x1, uint32_t x2);     /// recieve system messages callback
    // TODO define this prototype
    void              (*msg_process)(void);
} ucm_plugin_t;

// *********************************************************
//      DataBase plugins functionality
// *********************************************************

enum {
    UCM_FLAG_DB_READONLY    = 1 << 0,
    UCM_FLAG_DB_CREATENEW   = 1 << 1,
    UCM_FLAG_DB_NEEDCHECK   = 1 << 2,
    UCM_FLAG_DB_NEEDBACKUP  = 1 << 3,
};

enum {
    UCM_DBVAL_FAIL    = 0,
    UCM_DBVAL_U8      = 1,
    UCM_DBVAL_U16     = 2,
    UCM_DBVAL_U32     = 3,
    UCM_DBVAL_FLOAT   = 4,
    UCM_DBVAL_DOUBLE  = 5,
    UCM_DBVAL_ASCIIZ  = 6,
    UCM_DBVAL_WIDESZ  = 7,
    UCM_DBVAL_U8SZ    = 8,
    UCM_DBVAL_BLOB    = 9,
};

typedef struct {
    uint8_t type;
    union {
        uint8_t  u8Val;
        uint16_t u16Val;
        uint32_t u32Val;

        float    fVal;
        double   dVal;

        struct {
            union {
                char*       szaVal;
                wchar_t*    szwVal;
                u8char_t*   sz8Val;
            };
            size_t szSize;
        };
        struct {
           uint8_t* bVal;
           size_t   bSize;
        };
    };
} ucm_dbval_t;

typedef struct _dbkey_s {
    HCONTACT    contact;
    wchar_t*    pid;
    char*       setting;
} ucm_dbkey_t;

typedef struct {
    ucm_plugin_t core;

    // technical db functionality
    UCM_RET   (*db_open)  (char* file, uint32_t flags);
    UCM_RET   (*db_check) (void);
    UCM_RET   (*db_flush) (void);
    UCM_RET   (*db_close) (void);

    // hight-level API. Use app structures config with one API function
    ucm_dbval_t* (*get_setting) (HCONTACT       contact,
                                 wchar_t*       pid,
                                 const char*    pName,
                                 ucm_dbval_t    defVal);

    UCM_RET      (*set_setting) (HCONTACT       contact,
                                 ucm_plugin_t*  module,
                                 char*          setting,
                                 ucm_dbval_t*   value);
    //TODO Events, contacts, logs iterators
} ucm_plugdb_t;

// *********************************************************
//      Protocol plugins functionality
// *********************************************************

typedef uintptr_t ucm_conptr_t;

enum {
    UCM_FLAG_NETSTAT_OFF     = 0,
    UCM_FLAG_NETSTAT_ON      = 1,
    UCM_FLAG_NETSTAT_LISTEN  = 2
};

enum {
    UCM_FLAG_NET_IAMSERVERMODE = 1 << 0 ,
    UCM_FLAG_NET_FILETRANSFER  = 1 << 1 ,
    UCM_FLAG_NET_GROUP         = 1 << 2 ,
    UCM_FLAG_NET_CRYPT         = 1 << 3 ,
    UCM_FLAG_NET_COMPRESS      = 1 << 4 ,
    UCM_FLAG_NET_BROADCAST     = 1 << 5 ,
    UCM_FLAG_NET_MULTYSESSION  = 1 << 6 ,
    UCM_FLAG_NET_NOSERVER      = 1 << 7
};

typedef struct {
    ucm_plugin_t    core;
    uint32_t        flags;

    const char**    (*get_interface)    (void);
    ucm_conptr_t    (*connect)          (uintptr_t ctx);
    UCM_RET         (*disconnect)       (ucm_conptr_t* cptr);
    int             (*get_status)       (void);
} ucm_plugproto_t;

// *********************************************************
//      User interface plugins functionality
// *********************************************************

typedef struct {
    ucm_plugin_t core;
    // TODO
} ucm_plugui_t;

// *********************************************************
//      CHAT FUNCTIONALITY STRUCTURES
// *********************************************************

//typedef struct {
//    uint8_t sid;
//    ucm_plugin_t* input_chain;
//    ucm_plugin_t* output_chain;
//} ucm_session_t;
//

// *********************************************************
//      MAIN APPLICATION API STRUCTURE
// *********************************************************

/*! API structure. Provide for all plugins */
typedef struct _ucm_functions_s {
    /*! CORE infrastructure API */

    /*! memory functions */
    void*       (*malloc)           (size_t size);
    void*       (*zmalloc)          (size_t size);
    void*       (*calloc)           (size_t nmem, size_t size);
    void        (*free)             (void* obj);
    void        (*zmemory)          (void* mem, size_t size);
    int         (*realloc)          (void** mem, size_t size);
    char*       (*strdup)           (const char* str);

    /*! pthread API functions*/
    uintptr_t   (*thread_create)    (void* (*func)(void* ctx), void* ctx);
    int         (*thread_detach)    (uintptr_t tid);
    void        (*thread_exit)      (void* ret);
    int         (*thread_join)      (uintptr_t tid);

    uintptr_t   (*mutex_create)     (void);
    void        (*mutex_free)       (uintptr_t _mtx);
    int         (*mutex_lock)       (uintptr_t _mtx);
    int         (*mutex_unlock)     (uintptr_t _mtx);

    uintptr_t   (*cond_create)      (void);
    int         (*cond_lock)        (uintptr_t _cond);
    int         (*cond_unlock)      (uintptr_t _cond);
    void        (*cond_free)        (uintptr_t _cond);
    int         (*cond_wait)        (uintptr_t _cond);
    int         (*cond_signal)      (uintptr_t _cond);
    int         (*cond_broadcast)   (uintptr_t _cond);

    uintptr_t   (*rwlock_create)    (void);
    void        (*rwlock_free)      (uintptr_t _rwl);
    int         (*rwlock_rlock)     (uintptr_t _rwl);
    int         (*rwlock_wlock)     (uintptr_t _rwl);
    int         (*rwlock_unlock)    (uintptr_t _rwl);

    /* Unicode operations. USC4 and convertors */
    int64_t     (*U8toU32)      (u8char_t* str,   const int64_t str_len, u32char_t** ret);
    int64_t     (*U32toU8)      (u32char_t* str,  const int64_t str_len, u8char_t** ret);
    size_t      (*ustrlen)      (u32char_t* str);
    u32char_t*  (*ustrdup)      (u32char_t* str);
    int         (*ustrcmp)      (u32char_t* lstr, u32char_t* rstr);
    int         (*ustrcasecmp)  (u32char_t* lstr, u32char_t* rstr);
    int         (*ustrncmp)     (u32char_t* lstr, u32char_t* rst, size_t num);
    int         (*ustrncasecmp) (u32char_t* lstr, u32char_t* rstr, size_t num);
    void        (*ustrupcase)   (u32char_t* str);
    void        (*ustrlowcase)  (u32char_t* str);
    void        (*ustrcpy)      (u32char_t* dest, u32char_t* src);
    void        (*ustrncpy)     (u32char_t* dest, u32char_t* src, size_t num);
    void        (*ustrcat)      (u32char_t* dest, u32char_t* src);
    void        (*ustrncat)     (u32char_t* dest, u32char_t* src, size_t num);
    void        (*umstrcat)     (u32char_t* dest, unsigned num,  ...);
    u32char_t*  (*ustrchr)      (u32char_t* str,  u32char_t chr);
    u32char_t*  (*ustrrchr)     (u32char_t* str,  u32char_t chr);
    u32char_t*  (*ustrjoin)     (u32char_t* str1, u32char_t* str2);
    u32char_t*  (*umstrjoin)    (size_t     num,  ...);
    u32char_t*  (*ustrbrkjoin)  (u32char_t* str1, u32char_t* str2, u32char_t brk);
    u32char_t*  (*umstrbrkjoin) (u32char_t brk,   size_t num, ...);
    int64_t     (*ustrstr)      (u32char_t* str,  u32char_t* sstr);
    int64_t     (*ustrcasestr)  (u32char_t* str,  u32char_t* sstr);

    /*! low-level settings provider functions */
    int         (*get_int)      (ucm_object_t* obj, char* key, int def);
    int64_t     (*get_int64)    (ucm_object_t* obj, char* key, int64_t def);
    float       (*get_float)    (ucm_object_t* obj, char* key, float def);
    char*       (*get_str)      (ucm_object_t* obj, char* key, char* def);
    wchar_t*    (*get_wstr)     (ucm_object_t* obj, char* key, wchar_t* def);
    uintptr_t   (*get_blob)     (ucm_object_t* obj, char* key, size_t* size);
    void        (*set_int)      (ucm_object_t* obj, char* key, int value);
    void        (*set_int64)    (ucm_object_t* obj, char* key, int64_t value);
    void        (*set_float)    (ucm_object_t* obj, char* key, float value);
    void        (*set_str)      (ucm_object_t* obj, char* key, char* value);
    wchar_t*    (*set_wstr)     (ucm_object_t* obj, char* key, wchar_t* value);
    void        (*set_blob)     (ucm_object_t* obj, char* key, uintptr_t blob, size_t size);

    void        (*item_del)     (ucm_object_t* obj, char* key);

    /*! general queue access */
    int         (*mainloop_msg_send)    (uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2);
    ucm_ev_t*   (*mainloop_ev_alloc)    (int id);
    int         (*mainloop_ev_push)     (ucm_ev_t* event, uint32_t x1, uint32_t x2, void* sender);
    void        (*mainloop_ev_free)     (ucm_ev_t** event);
    void        (*mainloop_flush)       (void);

    void        (*mainloop_hook_attach) (void(*callback)(uint32_t id, uintptr_t ev_ctx, uint32_t x1, uint32_t x2, void* ctx), void* ctx, uint32_t mask);
    void        (*mainloop_hook_detach) (void(*callback)(uint32_t id, uintptr_t ev_ctx, uint32_t x1, uint32_t x2, void* ctx));

    /*! get MD5 hash function */
    void        (*md5)          (uint8_t buf[16], const char* in, int size);
    void        (*md5_to_str)   (char* out, const uint8_t buf[16]);

    /*! log and trace messages handlers*/
    void        (*log)              (ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...);
    void        (*ucm_log)          (const char* fmt, ...);
    void        (*logger_connect)   (void (*callback)(ucm_plugin_t*,uint32_t,const char*));
    void        (*logger_disconnect)(void (*callback)(ucm_plugin_t*,uint32_t,const char*));

    /*! get plugins by category */
    const ucm_plugin_t** (*get_plugins_all)   (void);
    const ucm_plugin_t** (*get_plugins_db)    (void);
    const ucm_plugin_t** (*get_plugins_proto) (void);
    const ucm_plugin_t** (*get_plugins_crypt) (void);
    const ucm_plugin_t** (*get_plugins_hist)  (void);
    const ucm_plugin_t** (*get_plugins_gui)   (void);
    const ucm_plugin_t** (*get_plugins_stuff) (void);

    /*! get global paths */
    const wchar_t* (*get_startup_path) (void);
    const wchar_t* (*get_store_path)   (void);
    const wchar_t* (*get_plugins_path) (void);

    /*! system entropy functions */
    int (*get_entropy) (void);

    /*! user API */
    const ucm_plugin_info_t*    (*get_plugin_info)  (char* pid);
    UCM_RET                     (*ucm_send_message) (void);   //TODO
    UCM_RET                     (*ucm_recv_message) (void);   //TODO

} ucm_functions_t;

typedef ucm_plugin_t*(*cb_init_plugin)(ucm_functions_t*);
// *********************************************************
//      START/STOP/INFO MODULE
// *********************************************************

enum {
    UCM_FLAG_CORE_DBNEW = 1 << 0,
    UCM_FLAG_CORE_DBRO  = 1 << 1,
};

// core start arguments
typedef struct {
    char*    path_abs       ;
    char*    path_plug_abs  ;
    char*    path_store_abs ;

    uint64_t options;
} ucm_cargs_t;

// ******* LOAD FUNCTIONS ************
LIBUCM_API const ucm_functions_t*
ucm_core_start (ucm_cargs_t* args);

LIBUCM_API UCM_RET
ucm_core_stop (void);

LIBUCM_API const ucm_plugin_info_t*
ucm_core_info (void);

// ******* DYNAMIC LOAD FUNCTIONS ***********

typedef const ucm_functions_t*
(*ucm_cstart_func) (ucm_cargs_t* args);
#define UCM_START_FUNC  "ucm_core_start"

typedef UCM_RET
(*ucm_cstop_func) (void);
#define UCM_STOP_FUNC  "ucm_core_stop"

typedef const ucm_plugin_info_t*
(*ucm_cinfo_func) (void);
#define UCM_INFO_FUNC  "ucm_core_info"

#undef UCM_DEPRECATED

#ifdef __cplusplus
    }
#endif
