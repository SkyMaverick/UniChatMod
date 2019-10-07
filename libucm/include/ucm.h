#pragma once

/* GhostCommander (gc) - simple, modular, scalable file manager */

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <wchar.h>

#ifndef STRICT_ASINC_API
    #if defined(ENABLE_CUSTOM_LIBS)
        #include "uv.h"
    #else
        #include <uv.h>
    #endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    // *********************************************************
    //      CORE STRUCTURES
    // *********************************************************

#include "uni/udefs.h" // preprocessor defines and includes. MUST BE FIRST !!!
#include "uni/umain.h" // base enums, structures and constants

#include "uni/usignals.h"  // signals
#include "uni/ucontacts.h" // contacts
#include "uni/uevents.h"   // events

    // *********************************************************
    //      PLUGINS FUNCTIONALITY API IMPLEMENTATION
    // *********************************************************

#include "uni/uplugin.h"    // plugins base
#include "uni/udatabase.h"  // database plugins functionality
#include "uni/uprotocols.h" // protocol plugins functionality
#include "uni/ugui.h"       // GUI plugin functionality

#ifndef STRICT_ASINC_API
    #include "uni/uuvapi.h" // libuv callbacks
#endif

    // *********************************************************
    //      MAIN APPLICATION API STRUCTURE
    // *********************************************************

    /*! API structure. Provide for all plugins */
    typedef struct _ucm_functions_s
    {
        struct
        {
            /*! memory functions */
            void* (*malloc)(size_t size);
            void* (*zmalloc)(size_t size);
            void* (*calloc)(size_t nmem, size_t size);
            void (*free)(void* obj);
            void (*zmemory)(void* mem, size_t size);
            int (*realloc)(void** mem, size_t size);
            void* (*memcpy)(void* mem, size_t size);
            char* (*strdup)(const char* str);
            char* (*strndup)(const char* str, size_t num);

            /*! pthread API functions*/
            uintptr_t (*thread_create)(void* (*func)(void* ctx), void* ctx);
            int (*thread_detach)(uintptr_t tid);
            void* (*thread_exit)(void);
            int (*thread_join)(uintptr_t tid);
            void (*thread_cleanup)(uintptr_t* tid);

            uintptr_t (*mutex_create)(void);
            void (*mutex_free)(uintptr_t _mtx);
            void (*mutex_lock)(uintptr_t _mtx);
            int (*mutex_trylock)(uintptr_t _mtx);
            void (*mutex_unlock)(uintptr_t _mtx);

            uintptr_t (*cond_create)(void);
            void (*cond_free)(uintptr_t _cond);
            void (*cond_wait)(uintptr_t _cond, uintptr_t _mtx);
            void (*cond_signal)(uintptr_t _cond);
            void (*cond_broadcast)(uintptr_t _cond);

            uintptr_t (*rwlock_create)(void);
            void (*rwlock_free)(uintptr_t _rwl);
            void (*rwlock_rlock)(uintptr_t _rwl);
            void (*rwlock_wlock)(uintptr_t _rwl);
            int (*rwlock_tryrlock)(uintptr_t _rwl);
            int (*rwlock_trywlock)(uintptr_t _rwl);
            void (*rwlock_unlock)(uintptr_t _rwl);

            uintptr_t (*dlopen)(const char* path);
            void (*dlclose)(uintptr_t lib);
            uintptr_t (*dlsym)(uintptr_t lib, const char* sym);
            const char* (*dlerror)(uintptr_t lib);

            int (*fs_fcreate)(const char* path);
            /* Unicode operations. USC4 and convertors */
            int64_t (*str2wstr)(ucm_str_t str, const int64_t str_len, ucm_wstr_t* ret);
            int64_t (*wstr2str)(ucm_wstr_t str, const int64_t str_len, ucm_str_t* ret);
            size_t (*ustrlen)(ucm_wstr_t str);
            ucm_wstr_t (*ustrdup)(ucm_wstr_t str);
            int (*ustrcmp)(ucm_wstr_t lstr, ucm_wstr_t rstr);
            int (*ustrcasecmp)(ucm_wstr_t lstr, ucm_wstr_t rstr);
            int (*ustrncmp)(ucm_wstr_t lstr, ucm_wstr_t rst, size_t num);
            int (*ustrncasecmp)(ucm_wstr_t lstr, ucm_wstr_t rstr, size_t num);
            void (*ustrupcase)(ucm_wstr_t str);
            void (*ustrlowcase)(ucm_wstr_t str);
            void (*ustrcpy)(ucm_wstr_t dest, ucm_wstr_t src);
            void (*ustrncpy)(ucm_wstr_t dest, ucm_wstr_t src, size_t num);
            void (*ustrcat)(ucm_wstr_t dest, ucm_wstr_t src);
            void (*ustrncat)(ucm_wstr_t dest, ucm_wstr_t src, size_t num);
            void (*umstrcat)(ucm_wstr_t dest, unsigned num, ...);
            ucm_wstr_t (*ustrchr)(ucm_wstr_t str, ucm_wchr_t chr);
            ucm_wstr_t (*ustrrchr)(ucm_wstr_t str, ucm_wchr_t chr);
            ucm_wstr_t (*ustrjoin)(ucm_wstr_t str1, ucm_wstr_t str2);
            ucm_wstr_t (*umstrjoin)(size_t num, ...);
            ucm_wstr_t (*ustrbrkjoin)(ucm_wstr_t str1, ucm_wstr_t str2, ucm_wchr_t brk);
            ucm_wstr_t (*umstrbrkjoin)(ucm_wchr_t brk, size_t num, ...);
            int64_t (*ustrstr)(ucm_wstr_t str, ucm_wstr_t sstr);
            int64_t (*ustrcasestr)(ucm_wstr_t str, ucm_wstr_t sstr);

            const char* (*strerr)(const unsigned errcode);

            void (*uuid_create)(ucm_uuid_t uuid);
            int (*uuid_parse)(const char* in, ucm_uuid_t uu);
            char* (*uuid_unparse)(const ucm_uuid_t uu);
            char* (*uuid_unparse_lower)(const ucm_uuid_t uu);
            char* (*uuid_unparse_upper)(const ucm_uuid_t uu);
            int (*uuid_is_null)(const ucm_uuid_t uu);
            int (*uuid_compare)(const ucm_uuid_t uu1, const ucm_uuid_t uu2);
        } sys;

        struct
        {
            void (*terminate)(uintptr_t ctx, uint32_t x1, uint32_t x2);
            void (*wait_exit)(void);
            uintptr_t (*get_loop)(int loop);
            int (*get_flag)(unsigned code);

            /*! low-level settings provider functions */
            int (*get_int)(ucm_object_t* obj, char* key, int def);
            int64_t (*get_int64)(ucm_object_t* obj, char* key, int64_t def);
            float (*get_float)(ucm_object_t* obj, char* key, float def);
            char* (*get_str)(ucm_object_t* obj, char* key, char* def);
            wchar_t* (*get_wstr)(ucm_object_t* obj, char* key, wchar_t* def);
            uintptr_t (*get_blob)(ucm_object_t* obj, char* key, size_t* size);
            void (*set_int)(ucm_object_t* obj, char* key, int value);
            void (*set_int64)(ucm_object_t* obj, char* key, int64_t value);
            void (*set_float)(ucm_object_t* obj, char* key, float value);
            void (*set_str)(ucm_object_t* obj, char* key, char* value);
            wchar_t* (*set_wstr)(ucm_object_t* obj, char* key, wchar_t* value);
            void (*set_blob)(ucm_object_t* obj, char* key, uintptr_t blob, size_t size);

            void (*item_del)(ucm_object_t* obj, char* key);

            /*! general queue access */
            int (*mainloop_msg_send)(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2);
            ucm_signal_t* (*mainloop_sig_alloc)(uint32_t id);
            ucm_signal_t* (*mainloop_sig_alloc2)(uint32_t id, void* ctx, size_t mem);
            int (*mainloop_sig_push)(ucm_signal_t* signal, uint32_t x1, uint32_t x2, void* sender);
            void (*mainloop_sig_free)(ucm_signal_t** signal);
            void (*mainloop_flush)(void);

            void (*mainloop_hook_attach)(void (*callback)(uint32_t id, uintptr_t ev_ctx,
                                                          uint32_t x1, uint32_t x2, void* ctx),
                                         void* ctx, uint32_t mask);
            void (*mainloop_hook_detach)(void (*callback)(uint32_t id, uintptr_t ev_ctx,
                                                          uint32_t x1, uint32_t x2, void* ctx));

            /*! get MD5 hash function */
            void (*md5)(uint8_t buf[16], const char* in, int size);
            void (*md5_to_str)(char* out, const uint8_t buf[16]);

            /*! log and trace messages handlers*/
            void (*log)(ucm_plugin_t* plugin, uint32_t type, const char* fmt, ...);
            void (*ucm_log)(const char* fmt, ...);
            void (*logger_connect)(void (*callback)(ucm_plugin_t*, uint32_t, const char*, void*),
                                   void* ctx);
            void (*logger_disconnect)(void (*callback)(ucm_plugin_t*, uint32_t, const char*,
                                                       void*));

            /*! get plugins by category */
            const ucm_plugin_t** (*plugins_by_type)(unsigned type);

            /*! get global paths */
            char* (*get_startup_path)(void);
            char* (*get_store_path)(void);
            char* (*get_plugins_path)(void);
            /*! system entropy functions */
            int (*get_entropy)(void);

            /*! user API */
            const ucm_plugin_info_t* (*get_plugin_info)(char* pid);
            UCM_RET (*ucm_send_message)(void); // TODO
            UCM_RET (*ucm_recv_message)(void); // TODO
        } app;
#ifndef STRICT_ASINC_API
        ucm_uv_t uv;
#endif
    } ucm_functions_t;

#define UCM_LOOP_SYSTEM(X) (uv_loop_t*)((X)->app.get_loop(CORE_LOOP_SYSTEM))
#define UCM_LOOP_NETWORK(X) (uv_loop_t*)((X)->app.get_loop(CORE_LOOP_NETWORK))

    typedef ucm_plugin_t* (*cb_init_plugin)(ucm_functions_t*);
    // *********************************************************
    //      START/STOP/INFO MODULE
    // *********************************************************

    // core start arguments
    typedef struct
    {
        char* path_abs;
        char* path_lib_abs;
        char* path_plug_abs;
        char* path_store_abs;

        uint64_t options;
    } ucm_cargs_t;

    enum
    {
        UCM_INFO_CORE    = 0,
        UCM_INFO_PLUGINS = 1,
    };

    // ******* LOAD FUNCTIONS ************

    LIBUCM_API const ucm_functions_t* ucm_core_start(ucm_cargs_t* args);

    LIBUCM_API UCM_RET ucm_core_stop(void);

    LIBUCM_API const size_t ucm_core_info(uint8_t mode, void* mem, size_t mem_size,
                                          ucm_cargs_t* args);

    // ******* DYNAMIC LOAD FUNCTIONS ***********

    typedef const ucm_functions_t* (*ucm_cstart_func)(ucm_cargs_t* args);
#define UCM_START_FUNC "ucm_core_start"

    typedef UCM_RET (*ucm_cstop_func)(void);
#define UCM_STOP_FUNC "ucm_core_stop"

    typedef const size_t (*ucm_cinfo_func)(uint8_t mode, void* mem, size_t mem_size,
                                           ucm_cargs_t* args);
#define UCM_INFO_FUNC "ucm_core_info"

#undef UCM_DEPRECATED

#ifdef __cplusplus
}
#endif
