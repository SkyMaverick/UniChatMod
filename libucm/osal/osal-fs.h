#pragma once

int osal_fs_close (uv_fs_t* req, uv_file file, uv_fs_cb cb);
int osal_fs_open (uv_fs_t* req, const char* path, int flags, int mode,uv_fs_cb cb);
int osal_fs_read (uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb);
int osal_fs_unlink (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_write (uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb);
int osal_fs_copyfile (uv_fs_t* req, const char* path, const char* new_path, int flags, uv_fs_cb cb);
int osal_fs_mkdir (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
int osal_fs_mkdtemp (uv_fs_t* req, const char* tpl, uv_fs_cb cb);
int osal_fs_rmdir (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_scandir (uv_fs_t* req, const char* path, int flags, uv_fs_cb cb);
int osal_fs_opendir (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_readdir(uv_fs_t* req, uv_dir_t* dir, uv_fs_cb cb);
int osal_fs_closedir(uv_fs_t* req, uv_dir_t* dir, uv_fs_cb cb);
int osal_fs_stat (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_fstat (uv_fs_t* req, uv_file file, uv_fs_cb cb);
int osal_fs_rename (uv_fs_t* req, const char* path, const char* new_path, uv_fs_cb cb);
int osal_fs_fsync (uv_fs_t* req, uv_file file, uv_fs_cb cb);
int osal_fs_fdatasync (uv_fs_t* req, uv_file file, uv_fs_cb cb);
int osal_fs_ftruncate (uv_fs_t* req, uv_file file, int64_t offset, uv_fs_cb cb);
int osal_fs_sendfile (uv_fs_t* req, uv_file out_fd, uv_file in_fd, int64_t in_offset, size_t length, uv_fs_cb cb);
int osal_fs_access (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
int osal_fs_chmod (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb);
int osal_fs_utime (uv_fs_t* req, const char* path, double atime, double mtime, uv_fs_cb cb);
int osal_fs_futime (uv_fs_t* req, uv_file file, double atime, double mtime, uv_fs_cb cb);
int osal_fs_lstat (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_link (uv_fs_t* req, const char* path, const char* new_path, uv_fs_cb cb);
int osal_fs_symlink (uv_fs_t* req, const char* path, const char* new_path, int flags, uv_fs_cb cb);
int osal_fs_readlink (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_realpath (uv_fs_t* req, const char* path, uv_fs_cb cb);
int osal_fs_fchmod (uv_fs_t* req, uv_file file, int mode, uv_fs_cb cb);
int osal_fs_chown (uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb);
int osal_fs_fchown (uv_fs_t* req, uv_file file, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb);
int osal_fs_lchown (uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb);
int osal_fs_event_init (uv_fs_event_t* handle);
int osal_fs_poll_init (uv_fs_poll_t* handle);

int osal_fs_fcreate (const char* path);
