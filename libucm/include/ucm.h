#pragma once

/* GhostCommander (gc) - simple, modular, scalable file manager */

#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <wchar.h>

#if defined(ENABLE_CUSTOM_LIBS)
    #include "uv.h"
#else
    #include <uv.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif

#include "ucm/udefs.h"   // preprocessor defines and includes. MUST BE FIRST !!!
#include "ucm/umain.h"   // base enums, structures and constants
#include "ucm/uevents.h" // events

    // *********************************************************
    //      CORE STRUCTURES
    // *********************************************************

    /*! Core return status */
    typedef enum _ucm_status_return_e
    {
        UCM_RET_SUCCESS = 0,
        UCM_RET_WRONGPARAM,
        UCM_RET_EXCEPTION,
        UCM_RET_NOTIMPLEMENT,
        UCM_RET_NOOBJECT,
        UCM_RET_EMPTY,
        UCM_RET_BUSY,

        UCM_RET_DATABASE_UNKWNERROR,
        UCM_RET_DATABASE_BADFORMAT,
        UCM_RET_DATABASE_BADINIT,
        UCM_RET_DATABASE_BADINTERNAL,
        UCM_RET_DATABASE_BADVERSION,
        UCM_RET_DATABASE_BADMETADATA,
        UCM_RET_DATABASE_NOTCHANGE,

        UCM_RET_SYSTEM_NOCREATE,
        UCM_RET_SYSTEM_NOACCESS,
        UCM_RET_SYSTEM_NOMEMORY,
        UCM_RET_SYSTEM_DLERROR,

        UCM_RET_MQUEUE_OVERFLOW,
        UCM_RET_MQUEUE_EMPTY,

        UCM_RET_PLUGIN_BADMODULE,
        UCM_RET_PLUGIN_BADVERSION,
        UCM_RET_PLUGIN_BADSYSTEM,
        UCM_RET_PLUGIN_BADPID,
        UCM_RET_PLUGIN_BADIFACE,

        UCM_RET_UNKNOWERROR
    } UCM_RET;
    // Messages and events ID  ---------------------

    // Messages -------------------
    enum
    {
        UCM_FLAG_MSG_MULTYCAST = 1 << 0,
        UCM_FLAG_MSG_CRYPTO    = 1 << 1,
        UCM_FLAG_MSG_ALERT     = 1 << 2
    };

    enum
    {
        UCM_TYPE_MSG_NORMAL = 0,
        UCM_TYPE_MSG_FILE   = 1,
        UCM_TYPE_MSG_ROOM   = 2,
        UCM_TYPE_MSG_STATUS = 3,
        UCM_TYPE_MSG_EVENT  = 4
    };

    typedef struct ucm_msg_s
    {
        ucm_object_t oid;

        uint8_t type;
        // TODO sender info
        time_t time;
        uint32_t flags;
        struct
        {
            size_t size;
            uint8_t blob[1];
        } data;
    } ucm_message_t;

    typedef uint64_t HCONTACT;

    typedef struct ucm_cont_s
    {
        ucm_object_t oid; // ucm system object ID

        HCONTACT cid; // global contact ID
        struct
        {
            char* name;
        } info;
        // TODO
    } ucm_contact_t;

    // Users ----------------------
    enum
    {
        UCM_USER_STATUS_INACTIVE   = 0,
        UCM_USER_STATUS_ACTIVE     = 1 << 0,
        UCM_USER_STATUS_ABSENT     = 1 << 1,
        UCM_USER_STATUS_NAVIALABLE = 1 << 2,
        UCM_USER_STATUS_BUSY       = 1 << 3,
        UCM_USER_STATUS_DNTDISTURB = 1 << 4,
        UCM_USER_STATUS_MAYTALK    = 1 << 5,
        UCM_USER_STATUS_INVISIBLE  = 1 << 6,
    };

    // *********************************************************
    //      PLUGINS FUNCTIONALITY API IMPLEMENTATION
    // *********************************************************

#define _EVENT_SENDER(obj) ((ucm_plugin_t*)(((ucm_ev_t*)obj)->sender))

    /*! Enums what defines the plugin area */
    enum
    {
        UCM_TYPE_PLUG_DB     = 1,
        UCM_TYPE_PLUG_PROTO  = 2,
        UCM_TYPE_PLUG_CRYPTO = 3,
        UCM_TYPE_PLUG_HIST   = 4,
        UCM_TYPE_PLUG_GUI    = 5,
        UCM_TYPE_PLUG_STUFF  = 6
    };

    /*! Usage API version */
    typedef struct
    {
        const uint8_t vmajor;
        const uint8_t vminor;
    } ucm_vapi_t;

    enum
    {
        UCM_FLAG_PLUG_LOGGED = 1,
    };

    typedef struct
    {
        const ucm_vapi_t api;  /// plugin release api version (required)
        const uint8_t sys;     /// plugin subsystem (required)
        const uint16_t vmajor; /// major plugin version (required)
        const uint16_t vminor; /// minor plugin version (required)
        const uint16_t vpatch; /// patch plugin version (required)
        uint32_t flags;        /// plugin flags
        // build info.
        struct
        {
            const wchar_t* commit;   /// commit in repository
            const wchar_t* datetime; /// build datetime
            const wchar_t* target;   /// build target platform
            const wchar_t* compiler; /// build this compiler
            const wchar_t* options;  /// build options
            const wchar_t* flags;    /// build with flags
        } build;
        const char* const pid; /// plugin id for internal ident (required)
        // developer info
        const wchar_t* const name;        /// plugin name for user
        const wchar_t* const developer;   /// developer name
        const wchar_t* const description; /// plugin description and more info
        const wchar_t* const copyright;   /// plugin license
        const wchar_t* const email;       /// support email
        const wchar_t* const website;     /// official website
    } ucm_plugin_info_t;

    /*! Structure what defines base plugin interface*/
    typedef struct _ucm_plugin_s
    {
        ucm_object_t oid; /// ucm system object identificator

        ucm_plugin_info_t info;
        UCM_RET(*run)
        (void);                /// activate plugin (with context for hot-plug) (required)
        UCM_RET (*stop)(void); /// deactivate plugin (required)
        void (*message)(uint32_t id, uintptr_t ctx, uint32_t x1,
                        uint32_t x2); /// recieve system messages callback
        // TODO define this prototype
        void (*msg_process)(void);
    } ucm_plugin_t;
#define U_PLUGIN(X) ((ucm_plugin_t*)(X))

    // *********************************************************
    //      DataBase plugins functionality
    // *********************************************************

    enum
    {
        UCM_DBVAL_FAIL   = 0,
        UCM_DBVAL_U8     = 1,
        UCM_DBVAL_U16    = 2,
        UCM_DBVAL_U32    = 3,
        UCM_DBVAL_FLOAT  = 4,
        UCM_DBVAL_DOUBLE = 5,
        UCM_DBVAL_ASCIIZ = 6,
        UCM_DBVAL_WIDESZ = 7,
        UCM_DBVAL_U8SZ   = 8,
        UCM_DBVAL_BLOB   = 9,
    };

    typedef struct
    {
        uint8_t type;
        union
        {
            uint8_t u8Val;
            uint16_t u16Val;
            uint32_t u32Val;

            float fVal;
            double dVal;

            struct
            {
                ucm_str_t sz8Val;
                size_t szSize;
            };
            struct
            {
                uint8_t* bVal;
                size_t bSize;
            };
        };
    } ucm_dbval_t;

    typedef struct _dbkey_s
    {
        HCONTACT contact;
        char* pid;
        char* setting;
    } ucm_dbkey_t;

    typedef struct
    {
        ucm_plugin_t core;

        // technical db functionality
        UCM_RET (*db_open)(void);
        UCM_RET (*db_check)(void);
        void (*db_flush)(bool force);
        UCM_RET (*db_close)(void);

        UCM_RET (*db_backup)(char* path);

        // hight-level API. Use app structures config with one API function
        ucm_dbval_t* (*get_setting)(HCONTACT contact, ucm_object_t* object, const char* setting,
                                    ucm_dbval_t* defVal);
        UCM_RET(*set_setting)
        (HCONTACT contact, ucm_object_t* object, const char* setting, ucm_dbval_t* value);
        // TODO Events, contacts, logs iterators
    } ucm_plugdb_t;

    // *********************************************************
    //      Protocol plugins functionality
    // *********************************************************

    typedef uintptr_t ucm_conptr_t;

    enum
    {
        UCM_FLAG_NETSTAT_OFF    = 0,
        UCM_FLAG_NETSTAT_ON     = 1,
        UCM_FLAG_NETSTAT_LISTEN = 2
    };

    enum
    {
        UCM_FLAG_NET_IAMSERVERMODE = 1 << 0,
        UCM_FLAG_NET_FILETRANSFER  = 1 << 1,
        UCM_FLAG_NET_GROUP         = 1 << 2,
        UCM_FLAG_NET_CRYPT         = 1 << 3,
        UCM_FLAG_NET_COMPRESS      = 1 << 4,
        UCM_FLAG_NET_BROADCAST     = 1 << 5,
        UCM_FLAG_NET_MULTYSESSION  = 1 << 6,
        UCM_FLAG_NET_NOSERVER      = 1 << 7
    };

    typedef struct
    {
        ucm_plugin_t core;
        uint32_t flags;

        const char** (*get_interface)(void);
        ucm_conptr_t (*connect)(uintptr_t ctx);
        UCM_RET (*disconnect)(ucm_conptr_t* cptr);
        int (*get_status)(void);
    } ucm_plugproto_t;

    // *********************************************************
    //      User interface plugins functionality
    // *********************************************************

    typedef struct
    {
        ucm_plugin_t core;
        // TODO
    } ucm_plugui_t;

    // *********************************************************
    //      CHAT FUNCTIONALITY STRUCTURES
    // *********************************************************

    // typedef struct {
    //    uint8_t sid;
    //    ucm_plugin_t* input_chain;
    //    ucm_plugin_t* output_chain;
    //} ucm_session_t;
    //

    // *********************************************************
    //      MAIN APPLICATION API STRUCTURE
    // *********************************************************

    /*! API structure. Provide for all plugins */
    typedef struct _ucm_functions_s
    {
        // LibUV API provided for all subsystem and plugins in UniAPI-functions
        struct
        {
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
            int (*write)(uv_write_t* req, uv_stream_t* handle, const uv_buf_t bufs[],
                         unsigned int nbufs, uv_write_cb cb);
            int (*write2)(uv_write_t* req, uv_stream_t* handle, const uv_buf_t bufs[],
                          unsigned int nbufs, uv_stream_t* send_handle, uv_write_cb cb);
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
            int (*udp_send)(uv_udp_send_t* req, uv_udp_t* handle, const uv_buf_t bufs[],
                            unsigned int nbufs, const struct sockaddr* addr,
                            uv_udp_send_cb send_cb);
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
            void (*pipe_connect)(uv_connect_t* req, uv_pipe_t* handle, const char* name,
                                 uv_connect_cb cb);
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
            int (*timer_start)(uv_timer_t* handle, uv_timer_cb cb, uint64_t timeout,
                               uint64_t repeat);
            int (*timer_stop)(uv_timer_t* handle);
            int (*timer_again)(uv_timer_t* handle);
            void (*timer_set_repeat)(uv_timer_t* handle, uint64_t repeat);
            uint64_t (*timer_get_repeat)(const uv_timer_t* handle);
            int (*getaddrinfo)(uv_loop_t* loop, uv_getaddrinfo_t* req,
                               uv_getaddrinfo_cb getaddrinfo_cb, const char* node,
                               const char* service, const struct addrinfo* hints);
            void (*freeaddrinfo)(struct addrinfo* ai);
            int (*getnameinfo)(uv_loop_t* loop, uv_getnameinfo_t* req,
                               uv_getnameinfo_cb getnameinfo_cb, const struct sockaddr* addr,
                               int flags);
            int (*spawn)(uv_loop_t* loop, uv_process_t* handle,
                         const uv_process_options_t* options);
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
            int (*fs_copyfile)(uv_loop_t* loop, uv_fs_t* req, const char* path,
                               const char* new_path, int flags, uv_fs_cb cb);
            int (*fs_mkdir)(uv_loop_t* loop, uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
            int (*fs_mkdtemp)(uv_loop_t* loop, uv_fs_t* req, const char* tpl, uv_fs_cb cb);
            int (*fs_rmdir)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
            int (*fs_scandir)(uv_loop_t* loop, uv_fs_t* req, const char* path, int flags,
                              uv_fs_cb cb);
            int (*fs_scandir_next)(uv_fs_t* req, uv_dirent_t* ent);
            int (*fs_stat)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
            int (*fs_fstat)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
            int (*fs_rename)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                             uv_fs_cb cb);
            int (*fs_fsync)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
            int (*fs_fdatasync)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
            int (*fs_ftruncate)(uv_loop_t* loop, uv_fs_t* req, uv_file file, int64_t offset,
                                uv_fs_cb cb);
            int (*fs_sendfile)(uv_loop_t* loop, uv_fs_t* req, uv_file out_fd, uv_file in_fd,
                               int64_t in_offset, size_t length, uv_fs_cb cb);
            int (*fs_access)(uv_loop_t* loop, uv_fs_t* req, const char* path, int mode,
                             uv_fs_cb cb);
            int (*fs_chmod)(uv_loop_t* loop, uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
            int (*fs_utime)(uv_loop_t* loop, uv_fs_t* req, const char* path, double atime,
                            double mtime, uv_fs_cb cb);
            int (*fs_futime)(uv_loop_t* loop, uv_fs_t* req, uv_file file, double atime,
                             double mtime, uv_fs_cb cb);
            int (*fs_lstat)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
            int (*fs_link)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                           uv_fs_cb cb);
            int (*fs_symlink)(uv_loop_t* loop, uv_fs_t* req, const char* path, const char* new_path,
                              int flags, uv_fs_cb cb);
            int (*fs_readlink)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
            int (*fs_realpath)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_fs_cb cb);
            int (*fs_fchmod)(uv_loop_t* loop, uv_fs_t* req, uv_file file, int mode, uv_fs_cb cb);
            int (*fs_chown)(uv_loop_t* loop, uv_fs_t* req, const char* path, uv_uid_t uid,
                            uv_gid_t gid, uv_fs_cb cb);
            int (*fs_fchown)(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_uid_t uid,
                             uv_gid_t gid, uv_fs_cb cb);
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
        } uv;
        /*! CORE infrastructure API */
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
        } sys;

        struct
        {
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
            ucm_ev_t* (*mainloop_ev_alloc)(uint32_t id);
            ucm_ev_t* (*mainloop_ev_alloc2)(uint32_t id, void* ctx, size_t mem);
            int (*mainloop_ev_push)(ucm_ev_t* event, uint32_t x1, uint32_t x2, void* sender);
            void (*mainloop_ev_free)(ucm_ev_t** event);
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
    } ucm_functions_t;

#define UCM_LOOP_SYSTEM(X) (uv_loop_t*)((X)->app.get_loop(CORE_LOOP_SYSTEM))
#define UCM_LOOP_NETWORK(X) (uv_loop_t*)((X)->app.get_loop(CORE_LOOP_NETWORK))

    typedef ucm_plugin_t* (*cb_init_plugin)(ucm_functions_t*);
    // *********************************************************
    //      START/STOP/INFO MODULE
    // *********************************************************

    enum
    {
        UCM_FLAG_MODE_NEWPROFILE = 1 << 0,
        UCM_FLAG_MODE_READONLY   = 1 << 1,
        UCM_FLAG_MODE_HEADLESS   = 1 << 2,
        UCM_FLAG_MODE_DAEMON     = 1 << 3
    };

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
