#include "ucm.h"
#include "api.h"

#include "logger.h"
#include "mainloop.h"
#include "threading.h"

char ucm_path[UCM_PATH_MAX];
char ucm_path_store[UCM_PATH_MAX];
char ucm_path_plugs[UCM_PATH_MAX];


// application global parameters (paths, vars, etc.)
static const char*
g_startup_path (void)
{
    return ucm_path;
}

static const char*
g_store_path (void)
{
    return ucm_path_store;
}

static const char*
g_plugins_path (void)
{
    return ucm_path_plugs;
}

static ucm_functions_t core_api = {
    .thread_create      = thread_create  ,
    .thread_detach      = thread_detach  ,
    .thread_exit        = thread_exit    ,
    .thread_join        = thread_join    ,

    .mutex_create       = mutex_create   ,
    .mutex_free         = mutex_free     ,
    .mutex_lock         = mutex_lock     ,
    .mutex_unlock       = mutex_unlock   ,

    .cond_create        = cond_create    ,
    .cond_free          = cond_free      ,
    .cond_wait          = cond_wait      ,
    .cond_signal        = cond_signal    ,
    .cond_broadcast     = cond_broadcast ,

    .rwlock_create      = rwlock_create  ,
    .rwlock_free        = rwlock_free    ,
    .rwlock_rlock       = rwlock_rlock   ,
    .rwlock_wlock       = rwlock_wlock   ,
    .rwlock_unlock      = rwlock_unlock  ,

    /*! settings provider functions */
    .get_int            = NULL,
    .get_int64          = NULL,
    .get_float          = NULL,
    .get_str            = NULL,
    .get_str_copy       = NULL,
    .set_int            = NULL,
    .set_int64          = NULL,
    .set_float          = NULL,
    .set_str            = NULL,
    .item_del           = NULL,

    /*! general queue access */
    .mainloop_msg_send  = ucm_mloop_push,
    .mainloop_ev_alloc  = ucm_mloop_event_alloc,
    .mainloop_ev_push   = ucm_mloop_event_push,
    .mainloop_ev_free   = ucm_mloop_event_free,
    .mainloop_flush     = NULL,

    /*! get MD5 hash function */
    .md5                = NULL,
    .md5_to_str         = NULL,

    /*! log and trace messages handlers*/
    .vlog               = logger_vlog       ,
    .log                = logger_log        ,
    .ucm_vlog           = ucm_vlog          ,
    .ucm_log            = ucm_log           ,
    .logger_connect     = logger_connect    ,
    .logger_disconnect  = logger_disconnect ,

    /*! get global paths */
    .get_startup_path   = g_startup_path ,
    .get_store_path     = g_store_path   ,
    .get_plugins_path   = g_plugins_path ,
};

ucm_functions_t* ucm_global_api = &core_api;
