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
    UCM_LOG_INFO = 1,
    UCM_LOG_DEBUG = 1 << 1,
    UCM_LOG_ERROR = 1 << 2
};

typedef struct {
    int ev;
    size_t size;
    void* sender;
} ucm_ev_t;

#define _EVENT_SENDER(obj)\
    ((ucm_plugin_t*)(((ucm_ev_t*)obj)->sender))

/*! Enums what defines the plugin area */
typedef enum _ucm_plugin_opt_e {
    UCM_PLUG_NET    = 2,
    UCM_PLUG_CRYPTO = 3,
    UCM_PLUG_HIST   = 4
} UCM_PLUG_TYPE;

/*! Usage API version */
typedef struct _ucm_plugin_api_version_s {
    const uint8_t vmajor;
    const uint8_t vminor;
} UCM_API_VER;

/*! Wrapper for global config access */
typedef struct{
    struct _ucm_confplug_s* config;
} UCM_CFG;

enum {
    UCM_FLAG_PLUGIN_LOGGED = 1,
};

/*! Structure what defines base plugin interface*/
typedef struct _ucm_plugin_s {
    const UCM_API_VER api;                 /// plugin release api version (required)
    const int type;                       /// plugin subsystem (required)
    const unsigned int vmajor;            /// major plugin version (required)
    const unsigned int vminor;            /// minor plugin version (required)
    // flags
    uint32_t flags;                       /// plugin flags
    // info
    char* const pid;                      /// plugin id for internal ident (required)
    char* const name;                     /// plugin name for user
    char* const developer;                /// developer name
    char* const description;              /// plugin description and more info
    char* const copyright;                /// plugin license
    char* const email;                    /// support email
    char* const website;                  /// official website

    UCM_RET (*run)(void);                 /// activate plugin (with context for hot-plug) (required)
    UCM_RET (*stop)(void);                /// deactivate plugin (required)
    void (*message)(uint32_t id, uintptr_t ctx, 
                    uint32_t x1, uint32_t x2);    /// recieve system messages callback
} ucm_plugin_t;

#undef UCM_DEPRECATED

#ifdef __cplusplus
    }
#endif

#endif /*_ucm_H_*/
