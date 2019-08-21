#include "osal-intrnl.h"

int osal_fs_close (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_close (LOOP_KRNL, req, file, cb);
}
int osal_fs_open (uv_fs_t* req, const char* path, int flags, int mode,uv_fs_cb cb) {
    return uv_fs_open (LOOP_KRNL, req, path, flags, mode, cb);
}
int osal_fs_read (uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb) {
    return uv_fs_read (LOOP_KRNL, req, file, bufs, nbufs, offset, cb);
}
int osal_fs_unlink (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_unlink (LOOP_KRNL, req, path, cb);
}
int osal_fs_write (uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb) {
    return uv_fs_write (LOOP_KRNL, req, file, bufs, nbufs, offset, cb);
}
int osal_fs_copyfile (uv_fs_t* req, const char* path, const char* new_path, int flags, uv_fs_cb cb) {
    return uv_fs_copyfile (LOOP_KRNL, req, path, new_path, flags, cb);
}
int osal_fs_mkdir (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb) {
    return uv_fs_mkdir (LOOP_KRNL, req, path, mode, cb);
}
int osal_fs_mkdtemp (uv_fs_t* req, const char* tpl, uv_fs_cb cb) {
    return uv_fs_mkdtemp (LOOP_KRNL, req, tpl, cb);
}
int osal_fs_rmdir (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_rmdir (LOOP_KRNL, req, path, cb);
}
int osal_fs_scandir (uv_fs_t* req, const char* path, int flags, uv_fs_cb cb) {
    return uv_fs_scandir (LOOP_KRNL, req, path, flags, cb);
}
int osal_fs_opendir (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return  uv_fs_opendir (LOOP_KRNL, req, path, cb);
}
int osal_fs_readdir (uv_fs_t* req, uv_dir_t* dir, uv_fs_cb cb) {
    return uv_fs_readdir(LOOP_KRNL, req, dir, cb);
}
int osal_fs_closedir(uv_fs_t* req, uv_dir_t* dir, uv_fs_cb cb) {
    return uv_fs_closedir (LOOP_KRNL, req, dir, cb);
}
int osal_fs_stat (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_stat (LOOP_KRNL, req, path, cb);
}
int osal_fs_fstat (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_fstat (LOOP_KRNL, req, file, cb);
}
int osal_fs_rename (uv_fs_t* req, const char* path, const char* new_path, uv_fs_cb cb) {
    return uv_fs_rename (LOOP_KRNL, req, path, new_path, cb);
}
int osal_fs_fsync (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_fsync (LOOP_KRNL, req, file, cb);
}
int osal_fs_fdatasync (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_fdatasync (LOOP_KRNL, req, file, cb);
}
int osal_fs_ftruncate (uv_fs_t* req, uv_file file, int64_t offset, uv_fs_cb cb) {
    return uv_fs_ftruncate (LOOP_KRNL, req, file, offset, cb);
}
int osal_fs_sendfile (uv_fs_t* req, uv_file out_fd, uv_file in_fd, int64_t in_offset, size_t length, uv_fs_cb cb) {
    return uv_fs_sendfile (LOOP_KRNL, req, out_fd, in_fd, in_offset, length, cb);
}
int osal_fs_access (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb) {
    return uv_fs_access (LOOP_KRNL, req, path, mode, cb);
}
int osal_fs_chmod (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb) {
    return uv_fs_chmod (LOOP_KRNL, req, path, mode, cb);
}
int osal_fs_utime (uv_fs_t* req, const char* path, double atime, double mtime, uv_fs_cb cb) {
    return uv_fs_utime (LOOP_KRNL, req, path, atime, mtime, cb);
}
int osal_fs_futime (uv_fs_t* req, uv_file file, double atime, double mtime, uv_fs_cb cb) {
    return uv_fs_futime (LOOP_KRNL, req, file, atime, mtime, cb);
}
int osal_fs_lstat (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_lstat (LOOP_KRNL, req, path, cb);
}
int osal_fs_link (uv_fs_t* req, const char* path, const char* new_path, uv_fs_cb cb) {
    return uv_fs_link (LOOP_KRNL, req, path, new_path, cb);
}
int osal_fs_symlink (uv_fs_t* req, const char* path, const char* new_path, int flags, uv_fs_cb cb) {
    return uv_fs_symlink (LOOP_KRNL, req, path, new_path, flags, cb);
}
int osal_fs_readlink (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_readlink (LOOP_KRNL, req, path, cb);
}
int osal_fs_realpath (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_realpath (LOOP_KRNL, req, path, cb);
}
int osal_fs_fchmod (uv_fs_t* req, uv_file file, int mode, uv_fs_cb cb) {
    return uv_fs_fchmod (LOOP_KRNL, req, file, mode, cb);
}
int osal_fs_chown (uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb) {
    return uv_fs_chown (LOOP_KRNL, req, path, uid, gid, cb);
}
int osal_fs_fchown (uv_fs_t* req, uv_file file, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb) {
    return uv_fs_fchown (LOOP_KRNL, req, file, uid, gid, cb);
}
int osal_fs_lchown (uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb) {
    return uv_fs_lchown (LOOP_KRNL, req, path, uid, gid, cb);
}
int osal_fs_event_init (uv_fs_event_t* handle) {
    return uv_fs_event_init (LOOP_KRNL, handle);
}
int osal_fs_poll_init (uv_fs_poll_t* handle) {
    return uv_fs_poll_init (LOOP_KRNL, handle);
}
