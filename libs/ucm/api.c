#include "api.h"
#include "ucm.h"

#include "core.h"
#include "cpentupd.h"
#include "errors.h"
#include "evhook.h"
#include "flags.h"
#include "logger.h"
#include "mainloop.h"
#include "osal/osal-intrnl.h"
#include "plugmgr.h"
#include "unicode.h"

#include <wchar.h>

char ucm_path[UCM_PATH_MAX];
char ucm_path_store[UCM_PATH_MAX];
char ucm_path_plugs[UCM_PATH_MAX];

// application global parameters (paths, vars, etc.)
static char*
g_startup_path(void) {
    return ucm_path;
}

static char*
g_store_path(void) {
    return ucm_path_store;
}

static char*
g_plugins_path(void) {
    return ucm_path_plugs;
}

static void
handle_terminate(uintptr_t ctx, uint32_t x1, uint32_t x2) {
    UniAPI->app.mainloop_msg_send(UCM_SIG_TERM, ctx, x1, x2);
}

static ucm_functions_t core_api = {.sys.malloc = osal_malloc,
                                   .sys.zmalloc = osal_zmalloc,
                                   .sys.calloc = osal_calloc,
                                   .sys.free = osal_free,
                                   .sys.zmemory = osal_zmemory,
                                   .sys.realloc = osal_realloc2,
                                   .sys.memdup = osal_memdup,
                                   .sys.strdup = osal_strdup,
                                   .sys.strndup = osal_strndup,
                                   .sys.thread_create = osal_thread_create,
                                   .sys.thread_detach = osal_thread_detach,
                                   .sys.thread_exit = osal_thread_exit,
                                   .sys.thread_join = osal_thread_join,
                                   .sys.thread_cleanup = osal_thread_cleanup,
                                   .sys.mutex_create = osal_mutex_create,
                                   .sys.mutex_free = osal_mutex_free,
                                   .sys.mutex_lock = osal_mutex_lock,
                                   .sys.mutex_trylock = osal_mutex_trylock,
                                   .sys.mutex_unlock = osal_mutex_unlock,
                                   .sys.cond_create = osal_cond_create,
                                   .sys.cond_free = osal_cond_free,
                                   .sys.cond_wait = osal_cond_wait,
                                   .sys.cond_signal = osal_cond_signal,
                                   .sys.cond_broadcast = osal_cond_broadcast,
                                   .sys.rwlock_create = osal_rwlock_create,
                                   .sys.rwlock_free = osal_rwlock_free,
                                   .sys.rwlock_rlock = osal_rwlock_rlock,
                                   .sys.rwlock_wlock = osal_rwlock_wlock,
                                   .sys.rwlock_tryrlock = osal_rwlock_tryrlock,
                                   .sys.rwlock_trywlock = osal_rwlock_trywlock,
                                   .sys.rwlock_unlock = osal_rwlock_unlock,
                                   .sys.dlopen = osal_dlopen,
                                   .sys.dlclose = osal_dlclose,
                                   .sys.dlsym = osal_dlsym,
                                   .sys.dlerror = osal_dlerror,
                                   .sys.fs_fcreate = osal_fs_fcreate,
                                   .sys.str2wstr = u8_decode_ucs4,
                                   .sys.wstr2str = ucs4_encode_u8,
                                   .sys.ustrlen = ucm_strlen,
                                   .sys.ustrdup = ucm_strdup2,
                                   .sys.ustrcmp = ucm_strcmp,
                                   .sys.ustrcasecmp = ucm_strcasecmp,
                                   .sys.ustrncmp = ucm_strncmp,
                                   .sys.ustrncasecmp = ucm_strncasecmp,
                                   .sys.ustrupcase = ucm_strupcase,
                                   .sys.ustrlowcase = ucm_strlowcase,
                                   .sys.ustrcpy = ucm_strcpy,
                                   .sys.ustrncpy = ucm_strncpy,
                                   .sys.ustrcat = ucm_strcat,
                                   .sys.ustrncat = ucm_strncat,
                                   .sys.umstrcat = ucm_mstrcat,
                                   .sys.ustrchr = ucm_strchr,
                                   .sys.ustrrchr = ucm_strrchr,
                                   .sys.ustrjoin = ucm_strjoin,
                                   .sys.umstrjoin = ucm_mstrjoin,
                                   .sys.ustrbrkjoin = ucm_strbrkjoin,
                                   .sys.umstrbrkjoin = ucm_mstrbrkjoin,
                                   .sys.ustrstr = ucm_strstr,
                                   .sys.ustrcasestr = ucm_strcasestr,
                                   .sys.strerr = ucm_strerr_func,
                                   .sys.uuid_create = osal_uuid_create,
                                   .sys.uuid_parse = osal_uuid_parse,
                                   .sys.uuid_unparse = osal_uuid_unparse,
                                   .sys.uuid_unparse_lower = osal_uuid_unparse_lower,
                                   .sys.uuid_unparse_upper = osal_uuid_unparse_upper,
                                   .sys.uuid_is_null = osal_uuid_is_null,
                                   .sys.uuid_compare = osal_uuid_compare,
                                   .app.terminate = handle_terminate,
                                   .app.wait_exit = wait_core_loop,
                                   .app.get_loop = get_loop_handle,
                                   .app.get_flag = get_system_flag,
                                   .app.get_int = NULL,
                                   .app.get_int64 = NULL,
                                   .app.get_float = NULL,
                                   .app.get_str = NULL,
                                   .app.get_wstr = NULL,
                                   .app.get_blob = NULL,
                                   .app.set_int = NULL,
                                   .app.set_int64 = NULL,
                                   .app.set_float = NULL,
                                   .app.set_str = NULL,
                                   .app.set_wstr = NULL,
                                   .app.set_blob = NULL,
                                   .app.item_del = NULL,
                                   .app.mainloop_msg_send = ucm_mloop_push,
                                   .app.mainloop_sig_alloc = ucm_signal_alloc,
                                   .app.mainloop_sig_alloc2 = ucm_signal_alloc2,
                                   .app.mainloop_sig_push = ucm_signal_push,
                                   .app.mainloop_sig_free = ucm_signal_free,
                                   .app.mainloop_flush = NULL,
                                   .app.mainloop_hook_attach = hooks_event_attach,
                                   .app.mainloop_hook_detach = hooks_event_detach,
                                   .app.md5 = NULL,
                                   .app.md5_to_str = NULL,
                                   .app.log = logger_log,
                                   .app.ucm_log = ucm_log,
                                   .app.logger_connect = logger_connect,
                                   .app.logger_disconnect = logger_disconnect,
                                   .app.plugins_by_type = pmgr_get,
                                   .app.get_startup_path = g_startup_path,
                                   .app.get_store_path = g_store_path,
                                   .app.get_plugins_path = g_plugins_path,
                                   .app.get_entropy = get_ucm_entropy,
                                   .app.get_plugin_info = NULL,
                                   .app.ucm_send_message = NULL,
                                   .app.ucm_recv_message = NULL,
#ifndef ONLY_WITH_CLIENT_API
                                   .uv.version = uv_version,
                                   .uv.version_string = uv_version_string,
                                   .uv.replace_allocator = uv_replace_allocator,
                                   .uv.default_loop = uv_default_loop,
                                   .uv.loop_init = uv_loop_init,
                                   .uv.loop_close = uv_loop_close,
                                   .uv.loop_new = uv_loop_new,
                                   .uv.loop_delete = uv_loop_delete,
                                   .uv.loop_size = uv_loop_size,
                                   .uv.loop_alive = uv_loop_alive,
                                   .uv.loop_configure = uv_loop_configure,
                                   .uv.loop_fork = uv_loop_fork,
                                   .uv.run = uv_run,
                                   .uv.stop = uv_stop,
                                   .uv.ref = uv_ref,
                                   .uv.unref = uv_unref,
                                   .uv.has_ref = uv_has_ref,
                                   .uv.update_time = uv_update_time,
                                   .uv.now = uv_now,
                                   .uv.backend_fd = uv_backend_fd,
                                   .uv.backend_timeout = uv_backend_timeout,
                                   .uv.translate_sys_error = uv_translate_sys_error,
                                   .uv.strerror = uv_strerror,
                                   .uv.err_name = uv_err_name,
                                   .uv.shutdown = uv_shutdown,
                                   .uv.handle_size = uv_handle_size,
                                   .uv.req_size = uv_req_size,
                                   .uv.is_active = uv_is_active,
                                   .uv.walk = uv_walk,
                                   .uv.print_all_handles = uv_print_all_handles,
                                   .uv.print_active_handles = uv_print_active_handles,
                                   .uv.close = uv_close,
                                   .uv.send_buffer_size = uv_send_buffer_size,
                                   .uv.recv_buffer_size = uv_recv_buffer_size,
                                   .uv.fileno = uv_fileno,
                                   .uv.buf_init = uv_buf_init,
                                   .uv.listen = uv_listen,
                                   .uv.accept = uv_accept,
                                   .uv.read_start = uv_read_start,
                                   .uv.read_stop = uv_read_stop,
                                   .uv.write = uv_write,
                                   .uv.write2 = uv_write2,
                                   .uv.try_write = uv_try_write,
                                   .uv.is_readable = uv_is_readable,
                                   .uv.is_writable = uv_is_writable,
                                   .uv.stream_set_blocking = uv_stream_set_blocking,
                                   .uv.is_closing = uv_is_closing,
                                   .uv.tcp_init = uv_tcp_init,
                                   .uv.tcp_init_ex = uv_tcp_init_ex,
                                   .uv.tcp_open = uv_tcp_open,
                                   .uv.tcp_nodelay = uv_tcp_nodelay,
                                   .uv.tcp_keepalive = uv_tcp_keepalive,
                                   .uv.tcp_simultaneous_accepts = uv_tcp_simultaneous_accepts,
                                   .uv.tcp_bind = uv_tcp_bind,
                                   .uv.tcp_getsockname = uv_tcp_getsockname,
                                   .uv.tcp_getpeername = uv_tcp_getpeername,
                                   .uv.tcp_connect = uv_tcp_connect,
                                   .uv.udp_init = uv_udp_init,
                                   .uv.udp_init_ex = uv_udp_init_ex,
                                   .uv.udp_open = uv_udp_open,
                                   .uv.udp_bind = uv_udp_bind,
                                   .uv.udp_getsockname = uv_udp_getsockname,
                                   .uv.udp_set_membership = uv_udp_set_membership,
                                   .uv.udp_set_multicast_loop = uv_udp_set_multicast_loop,
                                   .uv.udp_set_multicast_ttl = uv_udp_set_multicast_ttl,
                                   .uv.udp_set_multicast_interface = uv_udp_set_multicast_interface,
                                   .uv.udp_set_broadcast = uv_udp_set_broadcast,
                                   .uv.udp_set_ttl = uv_udp_set_ttl,
                                   .uv.udp_send = uv_udp_send,
                                   .uv.udp_try_send = uv_udp_try_send,
                                   .uv.udp_recv_start = uv_udp_recv_start,
                                   .uv.udp_recv_stop = uv_udp_recv_stop,
                                   .uv.tty_init = uv_tty_init,
                                   .uv.tty_set_mode = uv_tty_set_mode,
                                   .uv.tty_reset_mode = uv_tty_reset_mode,
                                   .uv.tty_get_winsize = uv_tty_get_winsize,
                                   .uv.guess_handle = uv_guess_handle,
                                   .uv.pipe_init = uv_pipe_init,
                                   .uv.pipe_open = uv_pipe_open,
                                   .uv.pipe_bind = uv_pipe_bind,
                                   .uv.pipe_connect = uv_pipe_connect,
                                   .uv.pipe_getsockname = uv_pipe_getsockname,
                                   .uv.pipe_getpeername = uv_pipe_getpeername,
                                   .uv.pipe_pending_instances = uv_pipe_pending_instances,
                                   .uv.pipe_pending_count = uv_pipe_pending_count,
                                   .uv.pipe_pending_type = uv_pipe_pending_type,
                                   .uv.pipe_chmod = uv_pipe_chmod,
                                   .uv.poll_init = uv_poll_init,
                                   .uv.poll_init_socket = uv_poll_init_socket,
                                   .uv.poll_start = uv_poll_start,
                                   .uv.poll_stop = uv_poll_stop,
                                   .uv.prepare_init = uv_prepare_init,
                                   .uv.prepare_start = uv_prepare_start,
                                   .uv.prepare_stop = uv_prepare_stop,
                                   .uv.check_init = uv_check_init,
                                   .uv.check_start = uv_check_start,
                                   .uv.check_stop = uv_check_stop,
                                   .uv.idle_init = uv_idle_init,
                                   .uv.idle_start = uv_idle_start,
                                   .uv.idle_stop = uv_idle_stop,
                                   .uv.async_init = uv_async_init,
                                   .uv.async_send = uv_async_send,
                                   .uv.timer_init = uv_timer_init,
                                   .uv.timer_start = uv_timer_start,
                                   .uv.timer_stop = uv_timer_stop,
                                   .uv.timer_again = uv_timer_again,
                                   .uv.timer_set_repeat = uv_timer_set_repeat,
                                   .uv.timer_get_repeat = uv_timer_get_repeat,
                                   .uv.getaddrinfo = uv_getaddrinfo,
                                   .uv.freeaddrinfo = uv_freeaddrinfo,
                                   .uv.getnameinfo = uv_getnameinfo,
                                   .uv.spawn = uv_spawn,
                                   .uv.process_kill = uv_process_kill,
                                   .uv.kill = uv_kill,
                                   .uv.queue_work = uv_queue_work,
                                   .uv.cancel = uv_cancel,
                                   .uv.setup_args = uv_setup_args,
                                   .uv.get_process_title = uv_get_process_title,
                                   .uv.set_process_title = uv_set_process_title,
                                   .uv.resident_set_memory = uv_resident_set_memory,
                                   .uv.uptime = uv_uptime,
                                   .uv.get_osfhandle = uv_get_osfhandle,
                                   .uv.getrusage = uv_getrusage,
                                   .uv.os_homedir = uv_os_homedir,
                                   .uv.os_tmpdir = uv_os_tmpdir,
                                   .uv.os_get_passwd = uv_os_get_passwd,
                                   .uv.os_free_passwd = uv_os_free_passwd,
                                   .uv.os_getpid = uv_os_getpid,
                                   .uv.os_getppid = uv_os_getppid,
                                   .uv.cpu_info = uv_cpu_info,
                                   .uv.free_cpu_info = uv_free_cpu_info,
                                   .uv.interface_addresses = uv_interface_addresses,
                                   .uv.free_interface_addresses = uv_free_interface_addresses,
                                   .uv.os_getenv = uv_os_getenv,
                                   .uv.os_setenv = uv_os_setenv,
                                   .uv.os_unsetenv = uv_os_unsetenv,
                                   .uv.os_gethostname = uv_os_gethostname,
                                   .uv.fs_req_cleanup = uv_fs_req_cleanup,
                                   .uv.fs_close = uv_fs_close,
                                   .uv.fs_open = uv_fs_open,
                                   .uv.fs_read = uv_fs_read,
                                   .uv.fs_unlink = uv_fs_unlink,
                                   .uv.fs_write = uv_fs_write,
                                   .uv.fs_copyfile = uv_fs_copyfile,
                                   .uv.fs_mkdir = uv_fs_mkdir,
                                   .uv.fs_mkdtemp = uv_fs_mkdtemp,
                                   .uv.fs_rmdir = uv_fs_rmdir,
                                   .uv.fs_scandir = uv_fs_scandir,
                                   .uv.fs_scandir_next = uv_fs_scandir_next,
                                   .uv.fs_stat = uv_fs_stat,
                                   .uv.fs_fstat = uv_fs_fstat,
                                   .uv.fs_rename = uv_fs_rename,
                                   .uv.fs_fsync = uv_fs_fsync,
                                   .uv.fs_fdatasync = uv_fs_fdatasync,
                                   .uv.fs_ftruncate = uv_fs_ftruncate,
                                   .uv.fs_sendfile = uv_fs_sendfile,
                                   .uv.fs_access = uv_fs_access,
                                   .uv.fs_chmod = uv_fs_chmod,
                                   .uv.fs_utime = uv_fs_utime,
                                   .uv.fs_futime = uv_fs_futime,
                                   .uv.fs_lstat = uv_fs_lstat,
                                   .uv.fs_link = uv_fs_link,
                                   .uv.fs_symlink = uv_fs_symlink,
                                   .uv.fs_readlink = uv_fs_readlink,
                                   .uv.fs_realpath = uv_fs_realpath,
                                   .uv.fs_fchmod = uv_fs_fchmod,
                                   .uv.fs_chown = uv_fs_chown,
                                   .uv.fs_fchown = uv_fs_fchown,
                                   .uv.fs_poll_init = uv_fs_poll_init,
                                   .uv.fs_poll_start = uv_fs_poll_start,
                                   .uv.fs_poll_stop = uv_fs_poll_stop,
                                   .uv.fs_poll_getpath = uv_fs_poll_getpath,
                                   .uv.signal_init = uv_signal_init,
                                   .uv.signal_start = uv_signal_start,
                                   .uv.signal_start_oneshot = uv_signal_start_oneshot,
                                   .uv.signal_stop = uv_signal_stop,
                                   .uv.loadavg = uv_loadavg,
                                   .uv.fs_event_init = uv_fs_event_init,
                                   .uv.fs_event_start = uv_fs_event_start,
                                   .uv.fs_event_stop = uv_fs_event_stop,
                                   .uv.fs_event_getpath = uv_fs_event_getpath,
                                   .uv.ip4_addr = uv_ip4_addr,
                                   .uv.ip6_addr = uv_ip6_addr,
                                   .uv.ip4_name = uv_ip4_name,
                                   .uv.ip6_name = uv_ip6_name,
                                   .uv.inet_ntop = uv_inet_ntop,
                                   .uv.inet_pton = uv_inet_pton,
                                   .uv.if_indextoname = uv_if_indextoname,
                                   .uv.if_indextoiid = uv_if_indextoiid,
                                   .uv.exepath = uv_exepath,
                                   .uv.cwd = uv_cwd,
                                   .uv.chdir = uv_chdir,
                                   .uv.get_free_memory = uv_get_free_memory,
                                   .uv.get_total_memory = uv_get_total_memory,
                                   .uv.hrtime = uv_hrtime,
                                   .uv.disable_stdio_inheritance = uv_disable_stdio_inheritance,
                                   .uv.key_create = uv_key_create,
                                   .uv.key_delete = uv_key_delete,
                                   .uv.key_get = uv_key_get,
                                   .uv.key_set = uv_key_set,
                                   .uv.thread_create = uv_thread_create,
                                   .uv.thread_self = uv_thread_self,
                                   .uv.thread_join = uv_thread_join,
                                   .uv.thread_equal = uv_thread_equal
#endif
};
ucm_functions_t* UniAPI = &core_api;
