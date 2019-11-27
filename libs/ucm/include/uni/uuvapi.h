#pragma once

#if defined(ENABLE_CUSTOM_LIBS)
    #include "uv.h"
#else
    #include <uv.h>
#endif

typedef struct {
    unsigned int (*version)(void);
    const char* (*version_string)(void);
    int (*replace_allocator)(uv_malloc_func malloc_func, uv_realloc_func realloc_func,
                             uv_calloc_func calloc_func, uv_free_func free_func);
    uv_loop_t* (*default_loop)(void);
    int (*loop_init)(uv_loop_t* loop);
    int (*loop_close)(uv_loop_t* loop);
    uv_loop_t* (*loop_new)(void);
    void (*loop_delete)(uv_loop_t*);
    size_t (*loop_size)(void);
    int (*loop_alive)(const uv_loop_t* loop);
    int (*loop_configure)(uv_loop_t* loop, uv_loop_option option, ...);
    int (*loop_fork)(uv_loop_t* loop);
    int (*run)(uv_loop_t*, uv_run_mode mode);
    void (*stop)(uv_loop_t*);
    void (*ref)(uv_handle_t*);
    void (*unref)(uv_handle_t*);
    int (*has_ref)(const uv_handle_t*);
    void (*update_time)(uv_loop_t*);
    uint64_t (*now)(const uv_loop_t*);
    int (*backend_fd)(const uv_loop_t*);
    int (*backend_timeout)(const uv_loop_t*);
    int (*translate_sys_error)(int sys_errno);
    const char* (*strerror)(int err);
    const char* (*err_name)(int err);
    int (*shutdown)(uv_shutdown_t* req, uv_stream_t* handle, uv_shutdown_cb cb);
    size_t (*handle_size)(uv_handle_type type);
    size_t (*req_size)(uv_req_type type);
    int (*is_active)(const uv_handle_t* handle);
    void (*walk)(uv_loop_t* loop, uv_walk_cb walk_cb, void* arg);
    void (*print_all_handles)(uv_loop_t* loop, FILE* stream);
    void (*print_active_handles)(uv_loop_t* loop, FILE* stream);
    void (*close)(uv_handle_t* handle, uv_close_cb close_cb);
    int (*send_buffer_size)(uv_handle_t* handle, int* value);
    int (*recv_buffer_size)(uv_handle_t* handle, int* value);
    int (*fileno)(const uv_handle_t* handle, uv_os_fd_t* fd);
    uv_buf_t (*buf_init)(char* base, unsigned int len);
    int (*listen)(uv_stream_t* stream, int backlog, uv_connection_cb cb);
    int (*accept)(uv_stream_t* server, uv_stream_t* client);
    int (*read_start)(uv_stream_t*, uv_alloc_cb alloc_cb, uv_read_cb read_cb);
    int (*read_stop)(uv_stream_t*);
    int (*write)(uv_write_t* req, uv_stream_t* handle, const uv_buf_t bufs[], unsigned int nbufs,
                 uv_write_cb cb);
    int (*write2)(uv_write_t* req, uv_stream_t* handle, const uv_buf_t bufs[], unsigned int nbufs,
                  uv_stream_t* send_handle, uv_write_cb cb);
    int (*try_write)(uv_stream_t* handle, const uv_buf_t bufs[], unsigned int nbufs);
    int (*is_readable)(const uv_stream_t* handle);
    int (*is_writable)(const uv_stream_t* handle);
    int (*stream_set_blocking)(uv_stream_t* handle, int blocking);
    int (*is_closing)(const uv_handle_t* handle);
    int (*tcp_init)(uv_loop_t*, uv_tcp_t* handle);
    int (*tcp_init_ex)(uv_loop_t*, uv_tcp_t* handle, unsigned int flags);
    int (*tcp_open)(uv_tcp_t* handle, uv_os_sock_t sock);
    int (*tcp_nodelay)(uv_tcp_t* handle, int enable);
    int (*tcp_keepalive)(uv_tcp_t* handle, int enable, unsigned int delay);
    int (*tcp_simultaneous_accepts)(uv_tcp_t* handle, int enable);
    int (*tcp_bind)(uv_tcp_t* handle, const struct sockaddr* addr, unsigned int flags);
    int (*tcp_getsockname)(const uv_tcp_t* handle, struct sockaddr* name, int* namelen);
    int (*tcp_getpeername)(const uv_tcp_t* handle, struct sockaddr* name, int* namelen);
    int (*tcp_connect)(uv_connect_t* req, uv_tcp_t* handle, const struct sockaddr* addr,
                       uv_connect_cb cb);
    int (*udp_init)(uv_loop_t*, uv_udp_t* handle);
    int (*udp_init_ex)(uv_loop_t*, uv_udp_t* handle, unsigned int flags);
    int (*udp_open)(uv_udp_t* handle, uv_os_sock_t sock);
    int (*udp_bind)(uv_udp_t* handle, const struct sockaddr* addr, unsigned int flags);
    int (*udp_getsockname)(const uv_udp_t* handle, struct sockaddr* name, int* namelen);
    int (*udp_set_membership)(uv_udp_t* handle, const char* multicast_addr,
                              const char* interface_addr, uv_membership membership);
    int (*udp_set_multicast_loop)(uv_udp_t* handle, int on);
    int (*udp_set_multicast_ttl)(uv_udp_t* handle, int ttl);
    int (*udp_set_multicast_interface)(uv_udp_t* handle, const char* interface_addr);
    int (*udp_set_broadcast)(uv_udp_t* handle, int on);
    int (*udp_set_ttl)(uv_udp_t* handle, int ttl);
    int (*udp_send)(uv_udp_send_t* req, uv_udp_t* handle, const uv_buf_t bufs[], unsigned int nbufs,
                    const struct sockaddr* addr, uv_udp_send_cb send_cb);
    int (*udp_try_send)(uv_udp_t* handle, const uv_buf_t bufs[], unsigned int nbufs,
                        const struct sockaddr* addr);
    int (*udp_recv_start)(uv_udp_t* handle, uv_alloc_cb alloc_cb, uv_udp_recv_cb recv_cb);
    int (*udp_recv_stop)(uv_udp_t* handle);
    int (*tty_init)(uv_loop_t*, uv_tty_t*, uv_file fd, int readable);
    int (*tty_set_mode)(uv_tty_t*, uv_tty_mode_t mode);
    int (*tty_reset_mode)(void);
    int (*tty_get_winsize)(uv_tty_t*, int* width, int* height);
    uv_handle_type (*guess_handle)(uv_file file);
    int (*pipe_init)(uv_loop_t*, uv_pipe_t* handle, int ipc);
    int (*pipe_open)(uv_pipe_t*, uv_file file);
    int (*pipe_bind)(uv_pipe_t* handle, const char* name);
    void (*pipe_connect)(uv_connect_t* req, uv_pipe_t* handle, const char* name, uv_connect_cb cb);
    int (*pipe_getsockname)(const uv_pipe_t* handle, char* buffer, size_t* size);
    int (*pipe_getpeername)(const uv_pipe_t* handle, char* buffer, size_t* size);
    void (*pipe_pending_instances)(uv_pipe_t* handle, int count);
    int (*pipe_pending_count)(uv_pipe_t* handle);
    uv_handle_type (*pipe_pending_type)(uv_pipe_t* handle);
    int (*pipe_chmod)(uv_pipe_t* handle, int flags);
    int (*poll_init)(uv_loop_t* loop, uv_poll_t* handle, int fd);
    int (*poll_init_socket)(uv_loop_t* loop, uv_poll_t* handle, uv_os_sock_t socket);
    int (*poll_start)(uv_poll_t* handle, int events, uv_poll_cb cb);
    int (*poll_stop)(uv_poll_t* handle);
    int (*prepare_init)(uv_loop_t*, uv_prepare_t* prepare);
    int (*prepare_start)(uv_prepare_t* prepare, uv_prepare_cb cb);
    int (*prepare_stop)(uv_prepare_t* prepare);
    int (*check_init)(uv_loop_t*, uv_check_t* check);
    int (*check_start)(uv_check_t* check, uv_check_cb cb);
    int (*check_stop)(uv_check_t* check);
    int (*idle_init)(uv_loop_t*, uv_idle_t* idle);
    int (*idle_start)(uv_idle_t* idle, uv_idle_cb cb);
    int (*idle_stop)(uv_idle_t* idle);
    int (*async_init)(uv_loop_t*, uv_async_t* async, uv_async_cb async_cb);
    int (*async_send)(uv_async_t* async);
    int (*timer_init)(uv_loop_t*, uv_timer_t* handle);
    int (*timer_start)(uv_timer_t* handle, uv_timer_cb cb, uint64_t timeout, uint64_t repeat);
    int (*timer_stop)(uv_timer_t* handle);
    int (*timer_again)(uv_timer_t* handle);
    void (*timer_set_repeat)(uv_timer_t* handle, uint64_t repeat);
    uint64_t (*timer_get_repeat)(const uv_timer_t* handle);
    int (*getaddrinfo)(uv_loop_t* loop, uv_getaddrinfo_t* req, uv_getaddrinfo_cb getaddrinfo_cb,
                       const char* node, const char* service, const struct addrinfo* hints);
    void (*freeaddrinfo)(struct addrinfo* ai);
    int (*getnameinfo)(uv_loop_t* loop, uv_getnameinfo_t* req, uv_getnameinfo_cb getnameinfo_cb,
                       const struct sockaddr* addr, int flags);
    int (*spawn)(uv_loop_t* loop, uv_process_t* handle, const uv_process_options_t* options);
    int (*process_kill)(uv_process_t*, int signum);
    int (*kill)(int pid, int signum);
    int (*queue_work)(uv_loop_t* loop, uv_work_t* req, uv_work_cb work_cb,
                      uv_after_work_cb after_work_cb);
    int (*cancel)(uv_req_t* req);
    char** (*setup_args)(int argc, char** argv);
    int (*get_process_title)(char* buffer, size_t size);
    int (*set_process_title)(const char* title);
    int (*resident_set_memory)(size_t* rss);
    int (*uptime)(double* uptime);
    uv_os_fd_t (*get_osfhandle)(int fd);
    int (*getrusage)(uv_rusage_t* rusage);
    int (*os_homedir)(char* buffer, size_t* size);
    int (*os_tmpdir)(char* buffer, size_t* size);
    int (*os_get_passwd)(uv_passwd_t* pwd);
    void (*os_free_passwd)(uv_passwd_t* pwd);
    uv_pid_t (*os_getpid)(void);
    uv_pid_t (*os_getppid)(void);
    int (*cpu_info)(uv_cpu_info_t** cpu_infos, int* count);
    void (*free_cpu_info)(uv_cpu_info_t* cpu_infos, int count);
    int (*interface_addresses)(uv_interface_address_t** addresses, int* count);
    void (*free_interface_addresses)(uv_interface_address_t* addresses, int count);
    int (*os_getenv)(const char* name, char* buffer, size_t* size);
    int (*os_setenv)(const char* name, const char* value);
    int (*os_unsetenv)(const char* name);
    int (*os_gethostname)(char* buffer, size_t* size);
    void (*fs_req_cleanup)(uv_fs_t* req);
    int (*fs_close)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
    int (*fs_open)(uv_loop_t* loop, uv_fs_t* req, const char* path, int flags, int mode,
                   uv_fs_cb cb);
    int (*fs_read)(uv_loop_t* loop, uv_fs_t* req, uv_file file, const uv_buf_t bufs[],
                   unsigned int nbufs, int64_t offset, uv_fs_cb cb);
    int (*fs_unlink)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
    int (*fs_write)(uv_loop_t* loop, uv_fs_t* req, uv_file file, const uv_buf_t bufs[],
                    unsigned int nbufs, int64_t offset, uv_fs_cb cb);
    int (*fs_copyfile)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                       int flags, uv_fs_cb cb);
    int (*fs_mkdir)(uv_loop_t* loop, uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
    int (*fs_mkdtemp)(uv_loop_t* loop, uv_fs_t* req, const char* tpl, uv_fs_cb cb);
    int (*fs_rmdir)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
    int (*fs_scandir)(uv_loop_t* loop, uv_fs_t* req, const char* path, int flags, uv_fs_cb cb);
    int (*fs_scandir_next)(uv_fs_t* req, uv_dirent_t* ent);
    int (*fs_stat)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
    int (*fs_fstat)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
    int (*fs_rename)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                     uv_fs_cb cb);
    int (*fs_fsync)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
    int (*fs_fdatasync)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
    int (*fs_ftruncate)(uv_loop_t* loop, uv_fs_t* req, uv_file file, int64_t offset, uv_fs_cb cb);
    int (*fs_sendfile)(uv_loop_t* loop, uv_fs_t* req, uv_file out_fd, uv_file in_fd,
                       int64_t in_offset, size_t length, uv_fs_cb cb);
    int (*fs_access)(uv_loop_t* loop, uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
    int (*fs_chmod)(uv_loop_t* loop, uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
    int (*fs_utime)(uv_loop_t* loop, uv_fs_t* req, const char* path, double atime, double mtime,
                    uv_fs_cb cb);
    int (*fs_futime)(uv_loop_t* loop, uv_fs_t* req, uv_file file, double atime, double mtime,
                     uv_fs_cb cb);
    int (*fs_lstat)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
    int (*fs_link)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                   uv_fs_cb cb);
    int (*fs_symlink)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                      int flags, uv_fs_cb cb);
    int (*fs_readlink)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
    int (*fs_realpath)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
    int (*fs_fchmod)(uv_loop_t* loop, uv_fs_t* req, uv_file file, int mode, uv_fs_cb cb);
    int (*fs_chown)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid,
                    uv_fs_cb cb);
    int (*fs_fchown)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_uid_t uid, uv_gid_t gid,
                     uv_fs_cb cb);
    int (*fs_poll_init)(uv_loop_t* loop, uv_fs_poll_t* handle);
    int (*fs_poll_start)(uv_fs_poll_t* handle, uv_fs_poll_cb poll_cb, const char* path,
                         unsigned int interval);
    int (*fs_poll_stop)(uv_fs_poll_t* handle);
    int (*fs_poll_getpath)(uv_fs_poll_t* handle, char* buffer, size_t* size);
    int (*signal_init)(uv_loop_t* loop, uv_signal_t* handle);
    int (*signal_start)(uv_signal_t* handle, uv_signal_cb signal_cb, int signum);
    int (*signal_start_oneshot)(uv_signal_t* handle, uv_signal_cb signal_cb, int signum);
    int (*signal_stop)(uv_signal_t* handle);
    void (*loadavg)(double avg[3]);
    int (*fs_event_init)(uv_loop_t* loop, uv_fs_event_t* handle);
    int (*fs_event_start)(uv_fs_event_t* handle, uv_fs_event_cb cb, const char* path,
                          unsigned int flags);
    int (*fs_event_stop)(uv_fs_event_t* handle);
    int (*fs_event_getpath)(uv_fs_event_t* handle, char* buffer, size_t* size);
    int (*ip4_addr)(const char* ip, int port, struct sockaddr_in* addr);
    int (*ip6_addr)(const char* ip, int port, struct sockaddr_in6* addr);
    int (*ip4_name)(const struct sockaddr_in* src, char* dst, size_t size);
    int (*ip6_name)(const struct sockaddr_in6* src, char* dst, size_t size);
    int (*inet_ntop)(int af, const void* src, char* dst, size_t size);
    int (*inet_pton)(int af, const char* src, void* dst);
    int (*if_indextoname)(unsigned int ifindex, char* buffer, size_t* size);
    int (*if_indextoiid)(unsigned int ifindex, char* buffer, size_t* size);
    int (*exepath)(char* buffer, size_t* size);
    int (*cwd)(char* buffer, size_t* size);
    int (*chdir)(const char* dir);
    uint64_t (*get_free_memory)(void);
    uint64_t (*get_total_memory)(void);
    uint64_t (*hrtime)(void);
    void (*disable_stdio_inheritance)(void);
    int (*key_create)(uv_key_t* key);
    void (*key_delete)(uv_key_t* key);
    void* (*key_get)(uv_key_t* key);
    void (*key_set)(uv_key_t* key, void* value);
    int (*thread_create)(uv_thread_t* tid, uv_thread_cb entry, void* arg);
    uv_thread_t (*thread_self)(void);
    int (*thread_join)(uv_thread_t* tid);
    int (*thread_equal)(const uv_thread_t* t1, const uv_thread_t* t2);
} ucm_uv_t;
