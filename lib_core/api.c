#include <wchar.h>

#include "ucm.h"
#include "osal.h"
#include "api.h"

#include "logger.h"
#include "mainloop.h"
#include "plugmgr.h"
#include "evhook.h"
#include "unicode.h"
#include "cpentupd.h"

wchar_t ucm_path        [UCM_PATH_MAX];
wchar_t ucm_path_store  [UCM_PATH_MAX];
wchar_t ucm_path_plugs  [UCM_PATH_MAX];
wchar_t ucm_path_doc    [UCM_PATH_MAX];

// application global parameters (paths, vars, etc.)
static const wchar_t*
g_startup_path (void)
{
    return ucm_path;
}

static const wchar_t*
g_store_path (void)
{
    return ucm_path_store;
}

static const wchar_t*
g_plugins_path (void)
{
    return ucm_path_plugs;
}

static ucm_functions_t core_api = {
    .malloc                 = osal_malloc            ,
    .zmalloc                = osal_zmalloc           ,
    .calloc                 = osal_calloc            ,
    .free                   = osal_free              ,
    .zmemory                = osal_zmemory           ,
    .realloc                = osal_realloc           ,
    .strdup                 = osal_strdup            ,

    .thread_create          = osal_thread_create     ,
    .thread_detach          = osal_thread_detach     ,
    .thread_exit            = osal_thread_exit       ,
    .thread_join            = osal_thread_join       ,

    .mutex_create           = osal_mutex_create      ,
    .mutex_free             = osal_mutex_free        ,
    .mutex_lock             = osal_mutex_lock        ,
    .mutex_unlock           = osal_mutex_unlock      ,

    .cond_create            = osal_cond_create       ,
    .cond_lock              = osal_cond_lock         ,
    .cond_unlock            = osal_cond_unlock       ,
    .cond_free              = osal_cond_free         ,
    .cond_wait              = osal_cond_wait         ,
    .cond_signal            = osal_cond_signal       ,
    .cond_broadcast         = osal_cond_broadcast    ,

    .rwlock_create          = osal_rwlock_create     ,
    .rwlock_free            = osal_rwlock_free       ,
    .rwlock_rlock           = osal_rwlock_rlock      ,
    .rwlock_wlock           = osal_rwlock_wlock      ,
    .rwlock_unlock          = osal_rwlock_unlock     ,

    .U8toU32                = u8_decode_ucs4        ,
    .U32toU8                = ucs4_encode_u8        ,
    .ustrlen                = ucm_strlen            ,
    .ustrdup                = ucm_strdup2           ,
    .ustrcmp                = ucm_strcmp            ,
    .ustrcasecmp            = ucm_strcasecmp        ,
    .ustrncmp               = ucm_strncmp           ,
    .ustrncasecmp           = ucm_strncasecmp       ,
    .ustrupcase             = ucm_strupcase         ,
    .ustrlowcase            = ucm_strlowcase        ,
    .ustrcpy                = ucm_strcpy            ,
    .ustrncpy               = ucm_strncpy           ,
    .ustrcat                = ucm_strcat            ,
    .ustrncat               = ucm_strncat           ,
    .umstrcat               = ucm_mstrcat           ,
    .ustrchr                = ucm_strchr            ,
    .ustrrchr               = ucm_strrchr           ,
    .ustrjoin               = ucm_strjoin           ,
    .umstrjoin              = ucm_mstrjoin          ,
    .ustrbrkjoin            = ucm_strbrkjoin        ,
    .umstrbrkjoin           = ucm_mstrbrkjoin       ,
    .ustrstr                = ucm_strstr            ,
    .ustrcasestr            = ucm_strcasestr        ,

    /*! settings provider functions */
    .get_int                = NULL                  ,
    .get_int64              = NULL                  ,
    .get_float              = NULL                  ,
    .get_str                = NULL                  ,
    .get_wstr               = NULL                  ,
    .get_blob               = NULL                  ,
    .set_int                = NULL                  ,
    .set_int64              = NULL                  ,
    .set_float              = NULL                  ,
    .set_str                = NULL                  ,
    .set_wstr               = NULL                  ,
    .set_blob               = NULL                  ,

    .item_del               = NULL                  ,

    /*! general queue access */
    .mainloop_msg_send      = ucm_mloop_push        ,
    .mainloop_ev_alloc      = ucm_mloop_event_alloc ,
    .mainloop_ev_push       = ucm_mloop_event_push  ,
    .mainloop_ev_free       = ucm_mloop_event_free  ,
    .mainloop_flush         = NULL,

    .mainloop_hook_attach   = hooks_event_attach    ,
    .mainloop_hook_detach   = hooks_event_detach    ,

    /*! get MD5 hash function */
    .md5                    = NULL                  ,
    .md5_to_str             = NULL                  ,

    /*! log and trace messages handlers*/
    .log                    = logger_log            ,
    .ucm_log                = ucm_log               ,
    .logger_connect         = logger_connect        ,
    .logger_disconnect      = logger_disconnect     ,

    .get_plugins_all        = plugins_get_all       ,
    .get_plugins_db         = plugins_get_db        ,
    .get_plugins_proto      = plugins_get_proto     ,
    .get_plugins_crypt      = plugins_get_crypt     ,
    .get_plugins_hist       = plugins_get_hist      ,
    .get_plugins_gui        = plugins_get_gui       ,
    .get_plugins_stuff      = plugins_get_stuff     ,

    .get_entropy            = get_ucm_entropy       ,

    /*! get global paths */
    .get_startup_path       = g_startup_path        ,
    .get_store_path         = g_store_path          ,
    .get_plugins_path       = g_plugins_path        ,
};

ucm_functions_t* UniAPI = &core_api;
