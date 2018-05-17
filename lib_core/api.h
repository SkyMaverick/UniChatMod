#ifndef _UCM_API_H_
#define _UCM_API_H_

#include "ucm.h"

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

#define UCM_PLUG_LIB    0
#define UCM_PLUG_STORE  1

/*! Core events identificator
  */
enum {
    UCM_EV_RUN,
    UCM_EV_TERM,
    UCM_EV_INFO,
    UCM_EV_LOAD_SUCCESS,
};

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

    /*! settings provider functions */
    int (*get_int) (char* group, char* key, int def);
    int64_t (*get_int64) (char* group, char* key, int64_t def);
    float (*get_float) (char* group, char* key, float def);
    char* (*get_str) (char* group, char* key, char* def);
    char* (*get_str_copy) (char* group, char* key, char* def);
    void (*set_int) (char* group, char* key, int value);
    void (*set_int64) (char* group, char* key, int64_t value);
    void (*set_float) (char* group, char* key, float value);
    void (*set_str) (char* group, char* key, char* value);
    void (*item_del) (char* group, char* key);
    
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
    void (*vlog)(ucm_plugin_t* plugin, uint32_t type, const char* fmt, va_list va);
    void (*log)(ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...);
    void (*ucm_vlog)(const char* fmt, va_list va);
    void (*ucm_log)(const char* fmt, ...);
    void (*logger_connect)(void (*callback)(ucm_plugin_t*,uint32_t,const char*));
    void (*logger_disconnect)(void (*callback)(ucm_plugin_t*,uint32_t,const char*));

    /*! get global paths */
    const char* const (*get_startup_path)(void);
    const char* const (*get_usercfg_path)(void);
    const char* const (*get_plugins_path)(void);
    const char* const (*get_socket_path)(void);
    const char* const (*get_locales_path)(void);
} ucm_functions_t;

extern ucm_functions_t* ucm_api;

#ifdef __cplusplus
    }
#endif

#endif
