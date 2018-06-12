#ifndef _UCM_H_
#define _UCM_H_

/* GhostCommander (gc) - simple, modular, scalable file manager */

#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>
#include <time.h>
#include <limits.h>

#ifdef __cplusplus
    extern "C" {
#endif

#define ENABLE_WARN_DEPRECATED 1

#define UCM_API_MAJOR_VER 0
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

#define UCM_PID_MAX 33

/* CORE structures *******************************************************
*/

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
    UCM_RET_BUSY
} UCM_RET;

enum {
    UCM_LOG_INFO  = 1 << 0,
    UCM_LOG_DEBUG = 1 << 1,
    UCM_LOG_ERROR = 1 << 2
};

/*! Core events identificator
  */
enum {
    UCM_EV_RUN,
    UCM_EV_TERM,
    UCM_EV_INFO,
    UCM_EV_LOAD_SUCCESS,
};

typedef struct {
    int ev;
    size_t size;
    void* sender;
} ucm_ev_t;

// ######################################################################
//      CHAT FUNCTIONALITY API IMPLEMENTATION
// ######################################################################

enum {
    UCM_FLAG_MSG_MULTYCAST  = 1 << 0,
    UCM_FLAG_MSG_CRYPTO     = 1 << 1,
    UCM_FLAG_MSG_ALERT      = 1 << 2
};

typedef struct ucm_cnt_info_s {
    uint32_t ip;
    const char* name;
    uint8_t status;
} ucm_cnt_info_t;

typedef struct ucm_msg_s {
    uint32_t flags;
    ucm_cnt_info_t info;
    // TODO
    // message body area
    char data[1];
} ucm_message_t;

// ######################################################################
//      PLUGINS FUNCTIONALITY API IMPLEMENTATION
// ######################################################################

#define _EVENT_SENDER(obj)\
    ((ucm_plugin_t*)(((ucm_ev_t*)obj)->sender))

/*! Enums what defines the plugin area */
typedef enum _ucm_plugin_opt_e {
    UCM_PLUG_DB       = 1 << 0,
    UCM_PLUG_NET      = 1 << 1,
    UCM_PLUG_CRYPTO   = 1 << 2,
    UCM_PLUG_HIST     = 1 << 3,
    UCM_PLUG_STUFF    = 1 << 4
} UCM_PLUG_TYPE;

/*! Usage API version */
typedef struct {
    const uint8_t vmajor;
    const uint8_t vminor;
} ucm_vapi_t;

enum {
    UCM_FLAG_PLUGIN_LOGGED = 1,
};

typedef struct {
    const ucm_vapi_t api;                 /// plugin release api version (required)
    const uint8_t sys;                    /// plugin subsystem (required)
    const uint16_t vmajor;                /// major plugin version (required)
    const uint16_t vminor;                /// minor plugin version (required)
    const uint16_t vpatch;                /// patch plugin version (required)
    const char* const pid;                /// plugin id for internal ident (required)
    uint32_t flags;                       /// plugin flags
    // build info.
    struct {
        const char* commit;               /// commit in repository
        const char* datetime;             /// build datetime
        const char* target;               /// build target platform
        const char* compiler;             /// build this compiler
        const char* options;              /// build options
        const char* flags;                /// build with flags
    } build;
    // developer info
    const char* const name;               /// plugin name for user
    const char* const developer;          /// developer name
    const char* const description;        /// plugin description and more info
    const char* const copyright;          /// plugin license
    const char* const email;              /// support email
    const char* const website;            /// official website
} ucm_plugin_info_t;

/*! Structure what defines base plugin interface*/
typedef struct _ucm_plugin_s {
    ucm_plugin_info_t info;
    UCM_RET (*run)(void);                 /// activate plugin (with context for hot-plug) (required)
    UCM_RET (*stop)(void);                /// deactivate plugin (required)
    void (*message)(uint32_t id, uintptr_t ctx,
                    uint32_t x1, uint32_t x2);    /// recieve system messages callback
} ucm_plugin_t;

typedef struct {
    ucm_plugin_t core;
    UCM_RET (*init_db)(void* ctx);
    // TODO
    UCM_RET (*close_db)(void);
} ucm_dbplugin_t;

// ######################################################################
//      MAIN APPLICATION API STRUCTURE
// ######################################################################

/*! API structure. Provide for all plugins */
typedef struct _ucm_functions_s {
    /*! CORE infrastructure API */
    /*! pthread API functions*/
    intptr_t (*thread_create)(void(*func)(void* ctx), void* ctx);
    int (*thread_detach)(intptr_t tid);
    void (*thread_exit)(void* ret);
    int (*thread_join)(intptr_t tid);

    uintptr_t (*mutex_create)(void);
    void (*mutex_free)(uintptr_t _mtx);
    int (*mutex_lock)(uintptr_t _mtx);
    int (*mutex_unlock)(uintptr_t _mtx);

    uintptr_t (*cond_create)(void);
    void (*cond_free)(uintptr_t _cond);
    int (*cond_wait)(uintptr_t _cond, uintptr_t _mtx);
    int (*cond_signal)(uintptr_t _cond);
    int (*cond_broadcast)(uintptr_t _cond);

    uintptr_t (*rwlock_create)(void);
    void (*rwlock_free)(uintptr_t _rwl);
    int (*rwlock_rlock)(uintptr_t _rwl);
    int (*rwlock_wlock)(uintptr_t _rwl);
    int (*rwlock_unlock)(uintptr_t _rwl);

    /*! low-level settings provider functions */
    int (*get_int) (char* key, int def);
    int64_t (*get_int64) (char* key, int64_t def);
    float (*get_float) (char* key, float def);
    char* (*get_str) (char* key, char* def);
    char* (*get_str_copy) (char* key, char* def);
    void (*set_int) (char* key, int value);
    void (*set_int64) (char* key, int64_t value);
    void (*set_float) (char* key, float value);
    void (*set_str) (char* key, char* value);
    void (*item_del) (char* key);

    /*! general queue access */
    int (*mainloop_msg_send)(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2);
    ucm_ev_t* (*mainloop_ev_alloc)(int id);
    int (*mainloop_ev_push)(ucm_ev_t* event, uint32_t x1, uint32_t x2, void* sender);
    void (*mainloop_ev_free)(ucm_ev_t** event);
    void (*mainloop_flush)(void);

    /*! get MD5 hash function */
    void (*md5)(uint8_t buf[16], const char* in, int size);
    void (*md5_to_str)(char* out, const uint8_t buf[16]);

    /*! log and trace messages handlers*/
    void (*log)(ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...);
    void (*ucm_log)(const char* fmt, ...);
    void (*logger_connect)(void (*callback)(ucm_plugin_t*,uint32_t,const char*));
    void (*logger_disconnect)(void (*callback)(ucm_plugin_t*,uint32_t,const char*));

    /*! get plugins by category */
    const ucm_plugin_t* (*get_plugins_all)   (void);
    const ucm_plugin_t* (*get_plugins_db)    (void);
    const ucm_plugin_t* (*get_plugins_net)   (void);
    const ucm_plugin_t* (*get_plugins_crypt) (void);
    const ucm_plugin_t* (*get_plugins_hist)  (void);
    const ucm_plugin_t* (*get_plugins_stuff) (void);

    /*! get global paths */
    const char* (*get_startup_path)(void);
    const char* (*get_store_path)(void);
    const char* (*get_plugins_path)(void);
    const char* (*get_doc_path)(void);

    /*! user API */
    const ucm_plugin_info_t* (*get_plugin_info)(char* pid);
    UCM_RET (*ucm_send_message)(void);   //TODO
    UCM_RET (*ucm_recv_message)(void);   //TODO

} ucm_functions_t;

// ######################################################################
//      START/STOP/INFO MODULE
// ######################################################################

const ucm_functions_t*
ucm_core_start (const char* path_abs,
                const char* path_store_abs);

UCM_RET
ucm_core_stop (void);

const ucm_plugin_info_t*
ucm_core_info (void);


#undef UCM_DEPRECATED

#ifdef __cplusplus
    }
#endif

#endif /*_ucm_H_*/
