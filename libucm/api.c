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
    .sys.malloc                 = osal_malloc            ,
    .sys.zmalloc                = osal_zmalloc           ,
    .sys.calloc                 = osal_calloc            ,
    .sys.free                   = osal_free              ,
    .sys.zmemory                = osal_zmemory           ,
    .sys.realloc                = osal_realloc2          ,
    .sys.strdup                 = osal_strdup            ,

    .sys.thread_create          = osal_thread_create     ,
    .sys.thread_detach          = osal_thread_detach     ,
    .sys.thread_exit            = osal_thread_exit       ,
    .sys.thread_join            = osal_thread_join       ,

    .sys.mutex_create           = osal_mutex_create      ,
    .sys.mutex_free             = osal_mutex_free        ,
    .sys.mutex_lock             = osal_mutex_lock        ,
    .sys.mutex_unlock           = osal_mutex_unlock      ,

    .sys.cond_create            = osal_cond_create       ,
    .sys.cond_lock              = osal_cond_lock         ,
    .sys.cond_unlock            = osal_cond_unlock       ,
    .sys.cond_free              = osal_cond_free         ,
    .sys.cond_wait              = osal_cond_wait         ,
    .sys.cond_signal            = osal_cond_signal       ,
    .sys.cond_broadcast         = osal_cond_broadcast    ,

    .sys.rwlock_create          = osal_rwlock_create     ,
    .sys.rwlock_free            = osal_rwlock_free       ,
    .sys.rwlock_rlock           = osal_rwlock_rlock      ,
    .sys.rwlock_wlock           = osal_rwlock_wlock      ,
    .sys.rwlock_unlock          = osal_rwlock_unlock     ,

    .sys.U8toU32                = u8_decode_ucs4        ,
    .sys.U32toU8                = ucs4_encode_u8        ,
    .sys.ustrlen                = ucm_strlen            ,
    .sys.ustrdup                = ucm_strdup2           ,
    .sys.ustrcmp                = ucm_strcmp            ,
    .sys.ustrcasecmp            = ucm_strcasecmp        ,
    .sys.ustrncmp               = ucm_strncmp           ,
    .sys.ustrncasecmp           = ucm_strncasecmp       ,
    .sys.ustrupcase             = ucm_strupcase         ,
    .sys.ustrlowcase            = ucm_strlowcase        ,
    .sys.ustrcpy                = ucm_strcpy            ,
    .sys.ustrncpy               = ucm_strncpy           ,
    .sys.ustrcat                = ucm_strcat            ,
    .sys.ustrncat               = ucm_strncat           ,
    .sys.umstrcat               = ucm_mstrcat           ,
    .sys.ustrchr                = ucm_strchr            ,
    .sys.ustrrchr               = ucm_strrchr           ,
    .sys.ustrjoin               = ucm_strjoin           ,
    .sys.umstrjoin              = ucm_mstrjoin          ,
    .sys.ustrbrkjoin            = ucm_strbrkjoin        ,
    .sys.umstrbrkjoin           = ucm_mstrbrkjoin       ,
    .sys.ustrstr                = ucm_strstr            ,
    .sys.ustrcasestr            = ucm_strcasestr        ,

    .app.get_int                = NULL                  ,
    .app.get_int64              = NULL                  ,
    .app.get_float              = NULL                  ,
    .app.get_str                = NULL                  ,
    .app.get_wstr               = NULL                  ,
    .app.get_blob               = NULL                  ,
    .app.set_int                = NULL                  ,
    .app.set_int64              = NULL                  ,
    .app.set_float              = NULL                  ,
    .app.set_str                = NULL                  ,
    .app.set_wstr               = NULL                  ,
    .app.set_blob               = NULL                  ,

    .app.item_del               = NULL                  ,

    .app.mainloop_msg_send      = ucm_mloop_push        ,
    .app.mainloop_ev_alloc      = ucm_mloop_event_alloc ,
    .app.mainloop_ev_push       = ucm_mloop_event_push  ,
    .app.mainloop_ev_free       = ucm_mloop_event_free  ,
    .app.mainloop_flush         = NULL,

    .app.mainloop_hook_attach   = hooks_event_attach    ,
    .app.mainloop_hook_detach   = hooks_event_detach    ,

    .app.md5                    = NULL                  ,
    .app.md5_to_str             = NULL                  ,

    .app.log                    = logger_log            ,
    .app.ucm_log                = ucm_log               ,
    .app.logger_connect         = logger_connect        ,
    .app.logger_disconnect      = logger_disconnect     ,

    .app.get_plugins_all        = plugins_get_all       ,
    .app.get_plugins_db         = plugins_get_db        ,
    .app.get_plugins_proto      = plugins_get_proto     ,
    .app.get_plugins_crypt      = plugins_get_crypt     ,
    .app.get_plugins_hist       = plugins_get_hist      ,
    .app.get_plugins_gui        = plugins_get_gui       ,
    .app.get_plugins_stuff      = plugins_get_stuff     ,

    .app.get_entropy            = get_ucm_entropy       ,

    .app.get_startup_path       = g_startup_path        ,
    .app.get_store_path         = g_store_path          ,
    .app.get_plugins_path       = g_plugins_path        ,
};

ucm_functions_t* UniAPI = &core_api;
