#include <wchar.h>

#include "ucm.h"
#include "api.h"
#include "core.h"
#include "logger.h"
#include "mainloop.h"
#include "plugmgr.h"
#include "evhook.h"
#include "unicode.h"
#include "cpentupd.h"

#include "errors.h"

#include "osal/osal-intrnl.h"

char ucm_path        [UCM_PATH_MAX];
char ucm_path_store  [UCM_PATH_MAX];
char ucm_path_plugs  [UCM_PATH_MAX];

uintptr_t
compat_layer_init(void)
{
    return osal_init();
}

void
compat_layer_release(void)
{
    osal_release();
}

// application global parameters (paths, vars, etc.)
static char*
g_startup_path (void)
{
    return ucm_path;
}

static char*
g_store_path (void)
{
    return ucm_path_store;
}

static char*
g_plugins_path (void)
{
    return ucm_path_plugs;
}

static ucm_functions_t core_api = {
    .uv.run                         = osal_run                       ,
    .uv.stop                        = uv_stop                        ,
    .uv.ref                         = uv_ref                         ,
    .uv.unref                       = uv_unref                       ,
    .uv.has_ref                     = uv_has_ref                     ,
    .uv.update_time                 = uv_update_time                 ,
    .uv.now                         = uv_now                         ,
    .uv.backend_fd                  = uv_backend_fd                  ,
    .uv.backend_timeout             = uv_backend_timeout             ,
    .uv.translate_sys_error         = uv_translate_sys_error         ,
    .uv.strerror                    = uv_strerror                    ,
    .uv.strerror_r                  = uv_strerror_r                  ,
    .uv.err_name                    = uv_err_name                    ,
    .uv.err_name_r                  = uv_err_name_r                  ,
    .uv.shutdown                    = uv_shutdown                    ,
    .uv.handle_size                 = uv_handle_size                 ,
    .uv.handle_get_type             = uv_handle_get_type             ,
    .uv.handle_type_name            = uv_handle_type_name            ,
    .uv.handle_get_data             = uv_handle_get_data             ,
    .uv.handle_get_loop             = uv_handle_get_loop             ,
    .uv.handle_set_data             = uv_handle_set_data             ,
    .uv.req_size                    = uv_req_size                    ,
    .uv.req_get_data                = uv_req_get_data                ,
    .uv.req_set_data                = uv_req_set_data                ,
    .uv.req_get_type                = uv_req_get_type                ,
    .uv.req_type_name               = uv_req_type_name               ,
    .uv.is_active                   = uv_is_active                   ,
    .uv.walk                        = uv_walk                        ,
    .uv.print_all_handles           = uv_print_all_handles           ,
    .uv.print_active_handles        = uv_print_active_handles        ,
    .uv.close                       = uv_close                       ,
    .uv.send_buffer_size            = uv_send_buffer_size            ,
    .uv.recv_buffer_size            = uv_recv_buffer_size            ,
    .uv.fileno                      = uv_fileno                      ,
    .uv.buf_init                    = uv_buf_init                    ,
    .uv.stream_get_write_queue_size = uv_stream_get_write_queue_size ,
    .uv.listen                      = uv_listen                      ,
    .uv.accept                      = uv_accept                      ,
    .uv.read_start                  = uv_read_start                  ,
    .uv.read_stop                   = uv_read_stop                   ,
    .uv.write                       = uv_write                       ,
    .uv.write2                      = uv_write2                      ,
    .uv.try_write                   = uv_try_write                   ,
    .uv.is_readable                 = uv_is_readable                 ,
    .uv.is_writable                 = uv_is_writable                 ,
    .uv.stream_set_blocking         = uv_stream_set_blocking         ,
    .uv.is_closing                  = uv_is_closing                  ,
    .uv.tcp_init                    = uv_tcp_init                    ,
    .uv.tcp_init_ex                 = uv_tcp_init_ex                 ,
    .uv.tcp_open                    = uv_tcp_open                    ,
    .uv.tcp_nodelay                 = uv_tcp_nodelay                 ,
    .uv.tcp_keepalive               = uv_tcp_keepalive               ,
    .uv.tcp_simultaneous_accepts    = uv_tcp_simultaneous_accepts    ,
    .uv.tcp_bind                    = uv_tcp_bind                    ,
    .uv.tcp_getsockname             = uv_tcp_getsockname             ,
    .uv.tcp_getpeername             = uv_tcp_getpeername             ,
    .uv.tcp_connect                 = uv_tcp_connect                 ,
    .uv.udp_init                    = uv_udp_init                    ,
    .uv.udp_init_ex                 = uv_udp_init_ex                 ,
    .uv.udp_open                    = uv_udp_open                    ,
    .uv.udp_bind                    = uv_udp_bind                    ,
    .uv.udp_connect                 = uv_udp_connect                 ,
    .uv.udp_getpeername             = uv_udp_getsockname             ,
    .uv.udp_getsockname             = uv_udp_getsockname             ,
    .uv.udp_set_membership          = uv_udp_set_membership          ,
    .uv.udp_set_multicast_loop      = uv_udp_set_multicast_loop      ,
    .uv.udp_set_multicast_ttl       = uv_udp_set_multicast_ttl       ,
    .uv.udp_set_multicast_interface = uv_udp_set_multicast_interface ,
    .uv.udp_set_broadcast           = uv_udp_set_broadcast           ,
    .uv.udp_set_ttl                 = uv_udp_set_ttl                 ,
    .uv.udp_send                    = uv_udp_send                    ,
    .uv.udp_try_send                = uv_udp_try_send                ,
    .uv.udp_recv_start              = uv_udp_recv_start              ,
    .uv.udp_recv_stop               = uv_udp_recv_stop               ,
    .uv.udp_get_send_queue_size     = uv_udp_get_send_queue_size     ,
    .uv.udp_get_send_queue_count    = uv_udp_get_send_queue_count    ,
    .uv.tty_init                    = uv_tty_init                    ,
    .uv.tty_set_mode                = uv_tty_set_mode                ,
    .uv.tty_reset_mode              = uv_tty_reset_mode              ,
    .uv.tty_get_winsize             = uv_tty_get_winsize             ,
    .uv.guess_handle                = uv_guess_handle                ,
    .uv.pipe_init                   = uv_pipe_init                   ,
    .uv.pipe_open                   = uv_pipe_open                   ,
    .uv.pipe_bind                   = uv_pipe_bind                   ,
    .uv.pipe_connect                = uv_pipe_connect                ,
    .uv.pipe_getsockname            = uv_pipe_getsockname            ,
    .uv.pipe_getpeername            = uv_pipe_getpeername            ,
    .uv.pipe_pending_instances      = uv_pipe_pending_instances      ,
    .uv.pipe_pending_count          = uv_pipe_pending_count          ,
    .uv.pipe_pending_type           = uv_pipe_pending_type           ,
    .uv.pipe_chmod                  = uv_pipe_chmod                  ,
    .uv.poll_init                   = uv_poll_init                   ,
    .uv.poll_init_socket            = uv_poll_init_socket            ,
    .uv.poll_start                  = uv_poll_start                  ,
    .uv.poll_stop                   = uv_poll_stop                   ,
    .uv.prepare_init                = uv_prepare_init                ,
    .uv.prepare_start               = uv_prepare_start               ,
    .uv.prepare_stop                = uv_prepare_stop                ,
    .uv.check_init                  = uv_check_init                  ,
    .uv.check_start                 = uv_check_start                 ,
    .uv.check_stop                  = uv_check_stop                  ,
    .uv.idle_init                   = uv_idle_init                   ,
    .uv.idle_start                  = uv_idle_start                  ,
    .uv.idle_stop                   = uv_idle_stop                   ,
    .uv.async_init                  = uv_async_init                  ,
    .uv.async_send                  = uv_async_send                  ,
    .uv.getaddrinfo                 = uv_getaddrinfo                 ,
    .uv.freeaddrinfo                = uv_freeaddrinfo                ,
    .uv.getnameinfo                 = uv_getnameinfo                 ,
    .uv.spawn                       = uv_spawn                       ,
    .uv.process_kill                = uv_process_kill                ,
    .uv.kill                        = uv_kill                        ,
    .uv.process_get_pid             = uv_process_get_pid             ,
    .uv.queue_work                  = uv_queue_work                  ,
    .uv.cancel                      = uv_cancel                      ,
    .uv.setup_args                  = uv_setup_args                  ,
    .uv.get_process_title           = uv_get_process_title           ,
    .uv.set_process_title           = uv_set_process_title           ,
    .uv.resident_set_memory         = uv_resident_set_memory         ,
    .uv.uptime                      = uv_uptime                      ,
    .uv.get_osfhandle               = uv_get_osfhandle               ,
    .uv.open_osfhandle              = uv_open_osfhandle              ,
    .uv.getrusage                   = uv_getrusage                   ,
    .uv.os_homedir                  = uv_os_homedir                  ,
    .uv.os_tmpdir                   = uv_os_tmpdir                   ,
    .uv.os_get_passwd               = uv_os_get_passwd               ,
    .uv.os_free_passwd              = uv_os_free_passwd              ,
    .uv.os_getpid                   = uv_os_getpid                   ,
    .uv.os_getppid                  = uv_os_getppid                  ,
    .uv.os_getpriority              = uv_os_getpriority              ,
    .uv.os_setpriority              = uv_os_setpriority              ,
    .uv.cpu_info                    = uv_cpu_info                    ,
    .uv.free_cpu_info               = uv_free_cpu_info               ,
    .uv.interface_addresses         = uv_interface_addresses         ,
    .uv.free_interface_addresses    = uv_free_interface_addresses    ,
    .uv.os_getenv                   = uv_os_getenv                   ,
    .uv.os_setenv                   = uv_os_setenv                   ,
    .uv.os_unsetenv                 = uv_os_unsetenv                 ,
    .uv.os_gethostname              = uv_os_gethostname              ,
    .uv.os_uname                    = uv_os_uname                    ,
    .uv.fs_get_type                 = uv_fs_get_type                 ,
    .uv.fs_get_result               = uv_fs_get_result               ,
    .uv.fs_get_ptr                  = uv_fs_get_ptr                  ,
    .uv.fs_get_path                 = uv_fs_get_path                 ,
    .uv.fs_get_statbuf              = uv_fs_get_statbuf              ,
    .uv.fs_req_cleanup              = uv_fs_req_cleanup              ,
    .uv.fs_close                    = osal_fs_close                  ,
    .uv.fs_open                     = osal_fs_open                   ,
    .uv.fs_read                     = osal_fs_read                   ,
    .uv.fs_unlink                   = osal_fs_unlink                 ,
    .uv.fs_write                    = osal_fs_write                  ,
    .uv.fs_copyfile                 = osal_fs_copyfile               ,
    .uv.fs_mkdir                    = osal_fs_mkdir                  ,
    .uv.fs_mkdtemp                  = osal_fs_mkdtemp                ,
    .uv.fs_rmdir                    = osal_fs_rmdir                  ,
    .uv.fs_scandir                  = osal_fs_scandir                ,
    .uv.fs_scandir_next             = uv_fs_scandir_next             ,
    .uv.fs_opendir                  = osal_fs_opendir                ,
    .uv.fs_readdir                  = osal_fs_readdir                ,
    .uv.fs_closedir                 = osal_fs_closedir               ,
    .uv.fs_stat                     = osal_fs_stat                   ,
    .uv.fs_fstat                    = osal_fs_fstat                  ,
    .uv.fs_rename                   = osal_fs_rename                 ,
    .uv.fs_fsync                    = osal_fs_fsync                  ,
    .uv.fs_fdatasync                = osal_fs_fdatasync              ,
    .uv.fs_ftruncate                = osal_fs_ftruncate              ,
    .uv.fs_sendfile                 = osal_fs_sendfile               ,
    .uv.fs_access                   = osal_fs_access                 ,
    .uv.fs_chmod                    = osal_fs_chmod                  ,
    .uv.fs_utime                    = osal_fs_utime                  ,
    .uv.fs_futime                   = osal_fs_futime                 ,
    .uv.fs_lstat                    = osal_fs_lstat                  ,
    .uv.fs_link                     = osal_fs_link                   ,
    .uv.fs_symlink                  = osal_fs_symlink                ,
    .uv.fs_readlink                 = osal_fs_readlink               ,
    .uv.fs_realpath                 = osal_fs_realpath               ,
    .uv.fs_fchmod                   = osal_fs_fchmod                 ,
    .uv.fs_chown                    = osal_fs_chown                  ,
    .uv.fs_fchown                   = osal_fs_fchown                 ,
    .uv.fs_lchown                   = osal_fs_lchown                 ,
    .uv.fs_poll_init                = osal_fs_poll_init              ,
    .uv.fs_poll_start               = uv_fs_poll_start             ,
    .uv.fs_poll_stop                = uv_fs_poll_stop              ,
    .uv.fs_poll_getpath             = uv_fs_poll_getpath           ,
    .uv.signal_init                 = uv_signal_init                 ,
    .uv.signal_start                = uv_signal_start                ,
    .uv.signal_start_oneshot        = uv_signal_start_oneshot        ,
    .uv.signal_stop                 = uv_signal_stop                 ,
    .uv.loadavg                     = uv_loadavg                     ,
    .uv.fs_event_init               = osal_fs_event_init               ,
    .uv.fs_event_start              = uv_fs_event_start              ,
    .uv.fs_event_stop               = uv_fs_event_stop               ,
    .uv.fs_event_getpath            = uv_fs_event_getpath            ,
    .uv.ip4_addr                    = uv_ip4_addr                    ,
    .uv.ip6_addr                    = uv_ip6_addr                    ,
    .uv.ip4_name                    = uv_ip4_name                    ,
    .uv.ip6_name                    = uv_ip6_name                    ,
    .uv.inet_ntop                   = uv_inet_ntop                   ,
    .uv.inet_pton                   = uv_inet_pton                   ,
    .uv.if_indextoname              = uv_if_indextoname              ,
    .uv.if_indextoiid               = uv_if_indextoiid               ,
    .uv.exepath                     = uv_exepath                     ,
    .uv.cwd                         = uv_cwd                         ,
    .uv.chdir                       = uv_chdir                       ,
    .uv.get_free_memory             = uv_get_free_memory             ,
    .uv.get_total_memory            = uv_get_total_memory            ,
    .uv.hrtime                      = uv_hrtime                      ,
    .uv.disable_stdio_inheritance   = uv_disable_stdio_inheritance   ,
    .uv.loop_get_data               = uv_loop_get_data               ,
    .uv.loop_set_data               = uv_loop_set_data               ,

    .sys.malloc                     = osal_malloc            ,
    .sys.zmalloc                    = osal_zmalloc           ,
    .sys.calloc                     = osal_calloc            ,
    .sys.free                       = osal_free              ,
    .sys.zmemory                    = osal_zmemory           ,
    .sys.realloc                    = osal_realloc2          ,
    .sys.strdup                     = osal_strdup            ,
    .sys.strndup                    = osal_strndup           ,

    .sys.thread_create              = osal_thread_create     ,
    .sys.thread_detach              = osal_thread_detach     ,
    .sys.thread_exit                = osal_thread_exit       ,
    .sys.thread_join                = osal_thread_join       ,
    .sys.thread_cleanup             = osal_thread_cleanup    ,

    .sys.mutex_create               = osal_mutex_create      ,
    .sys.mutex_free                 = osal_mutex_free        ,
    .sys.mutex_lock                 = osal_mutex_lock        ,
    .sys.mutex_trylock              = osal_mutex_trylock     ,
    .sys.mutex_unlock               = osal_mutex_unlock      ,

    .sys.cond_create                = osal_cond_create       ,
    .sys.cond_free                  = osal_cond_free         ,
    .sys.cond_wait                  = osal_cond_wait         ,
    .sys.cond_signal                = osal_cond_signal       ,
    .sys.cond_broadcast             = osal_cond_broadcast    ,

    .sys.rwlock_create              = osal_rwlock_create     ,
    .sys.rwlock_free                = osal_rwlock_free       ,
    .sys.rwlock_rlock               = osal_rwlock_rlock      ,
    .sys.rwlock_wlock               = osal_rwlock_wlock      ,
    .sys.rwlock_tryrlock            = osal_rwlock_tryrlock   ,
    .sys.rwlock_trywlock            = osal_rwlock_trywlock   ,
    .sys.rwlock_unlock              = osal_rwlock_unlock     ,

    .sys.dlopen                     = osal_dlopen            ,
    .sys.dlclose                    = osal_dlclose           ,
    .sys.dlsym                      = osal_dlsym             ,
    .sys.dlerror                    = osal_dlerror           ,

    .sys.timer_create               = osal_timer_create      ,
    .sys.timer_start                = osal_timer_start       ,
    .sys.timer_stop                 = osal_timer_stop        ,
    .sys.timer_again                = osal_timer_again       ,
    .sys.timer_set_repeat           = osal_timer_set_repeat  ,
    .sys.timer_get_repeat           = osal_timer_get_repeat  ,
    .sys.timer_release              = osal_timer_release     ,

    .sys.os_errno                   = osal_errno             ,
    .sys.fs_fcreate                 = osal_fs_fcreate        ,

    .sys.U8toU32                    = u8_decode_ucs4         ,
    .sys.U32toU8                    = ucs4_encode_u8         ,
    .sys.ustrlen                    = ucm_strlen             ,
    .sys.ustrdup                    = ucm_strdup2            ,
    .sys.ustrcmp                    = ucm_strcmp             ,
    .sys.ustrcasecmp                = ucm_strcasecmp         ,
    .sys.ustrncmp                   = ucm_strncmp            ,
    .sys.ustrncasecmp               = ucm_strncasecmp        ,
    .sys.ustrupcase                 = ucm_strupcase          ,
    .sys.ustrlowcase                = ucm_strlowcase         ,
    .sys.ustrcpy                    = ucm_strcpy             ,
    .sys.ustrncpy                   = ucm_strncpy            ,
    .sys.ustrcat                    = ucm_strcat             ,
    .sys.ustrncat                   = ucm_strncat            ,
    .sys.umstrcat                   = ucm_mstrcat            ,
    .sys.ustrchr                    = ucm_strchr             ,
    .sys.ustrrchr                   = ucm_strrchr            ,
    .sys.ustrjoin                   = ucm_strjoin            ,
    .sys.umstrjoin                  = ucm_mstrjoin           ,
    .sys.ustrbrkjoin                = ucm_strbrkjoin         ,
    .sys.umstrbrkjoin               = ucm_mstrbrkjoin        ,
    .sys.ustrstr                    = ucm_strstr             ,
    .sys.ustrcasestr                = ucm_strcasestr         ,

    .sys.strerr                     = ucm_strerr_func        ,

    .app.wait_exit                  = wait_core_loop         ,

    .app.get_int                    = NULL                   ,
    .app.get_int64                  = NULL                   ,
    .app.get_float                  = NULL                   ,
    .app.get_str                    = NULL                   ,
    .app.get_wstr                   = NULL                   ,
    .app.get_blob                   = NULL                   ,
    .app.set_int                    = NULL                   ,
    .app.set_int64                  = NULL                   ,
    .app.set_float                  = NULL                   ,
    .app.set_str                    = NULL                   ,
    .app.set_wstr                   = NULL                   ,
    .app.set_blob                   = NULL                   ,

    .app.item_del                   = NULL                   ,

    .app.mainloop_msg_send          = ucm_mloop_push         ,
    .app.mainloop_ev_alloc          = ucm_event_alloc        ,
    .app.mainloop_ev_alloc2         = ucm_event_alloc2       ,
    .app.mainloop_ev_push           = ucm_event_push         ,
    .app.mainloop_ev_free           = ucm_event_free         ,
    .app.mainloop_flush             = NULL,

    .app.mainloop_hook_attach       = hooks_event_attach     ,
    .app.mainloop_hook_detach       = hooks_event_detach     ,

    .app.md5                        = NULL                   ,
    .app.md5_to_str                 = NULL                   ,

    .app.log                        = logger_log             ,
    .app.ucm_log                    = ucm_log                ,
    .app.logger_connect             = logger_connect         ,
    .app.logger_disconnect          = logger_disconnect      ,

    .app.plugins_by_type            = pmgr_get               ,

    .app.get_entropy                = get_ucm_entropy        ,

    .app.get_startup_path           = g_startup_path         ,
    .app.get_store_path             = g_store_path           ,
    .app.get_plugins_path           = g_plugins_path         ,
};

ucm_functions_t* UniAPI = &core_api;
