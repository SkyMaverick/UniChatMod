#include "osal-intrnl.h"

int osal_fs_close (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_close (o_krnl->loop_system, req, file, cb);
}
int osal_fs_open (uv_fs_t* req, const char* path, int flags, int mode,uv_fs_cb cb) {
    return uv_fs_open (o_krnl->loop_system, req, path, flags, mode, cb);
}
int osal_fs_read (uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb) {
    return uv_fs_read (o_krnl->loop_system, req, file, bufs, nbufs, offset, cb);
}
int osal_fs_unlink (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_unlink (o_krnl->loop_system, req, path, cb);
}
int osal_fs_write (uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb) {
    return uv_fs_write (o_krnl->loop_system, req, file, bufs, nbufs, offset, cb);
}
int osal_fs_copyfile (uv_fs_t* req, const char* path, const char* new_path, int flags, uv_fs_cb cb) {
    return uv_fs_copyfile (o_krnl->loop_system, req, path, new_path, flags, cb);
}
int osal_fs_mkdir (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb) {
    return uv_fs_mkdir (o_krnl->loop_system, req, path, mode, cb);
}
int osal_fs_mkdtemp (uv_fs_t* req, const char* tpl, uv_fs_cb cb) {
    return uv_fs_mkdtemp (o_krnl->loop_system, req, tpl, cb);
}
int osal_fs_rmdir (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_rmdir (o_krnl->loop_system, req, path, cb);
}
int osal_fs_scandir (uv_fs_t* req, const char* path, int flags, uv_fs_cb cb) {
    return uv_fs_scandir (o_krnl->loop_system, req, path, flags, cb);
}
int osal_fs_opendir (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return  uv_fs_opendir (o_krnl->loop_system, req, path, cb);
}
int osal_fs_readdir (uv_fs_t* req, uv_dir_t* dir, uv_fs_cb cb) {
    return uv_fs_readdir(o_krnl->loop_system, req, dir, cb);
}
int osal_fs_closedir(uv_fs_t* req, uv_dir_t* dir, uv_fs_cb cb) {
    return uv_fs_closedir (o_krnl->loop_system, req, dir, cb);
}
int osal_fs_stat (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_stat (o_krnl->loop_system, req, path, cb);
}
int osal_fs_fstat (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_fstat (o_krnl->loop_system, req, file, cb);
}
int osal_fs_rename (uv_fs_t* req, const char* path, const char* new_path, uv_fs_cb cb) {
    return uv_fs_rename (o_krnl->loop_system, req, path, new_path, cb);
}
int osal_fs_fsync (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_fsync (o_krnl->loop_system, req, file, cb);
}
int osal_fs_fdatasync (uv_fs_t* req, uv_file file, uv_fs_cb cb) {
    return uv_fs_fdatasync (o_krnl->loop_system, req, file, cb);
}
int osal_fs_ftruncate (uv_fs_t* req, uv_file file, int64_t offset, uv_fs_cb cb) {
    return uv_fs_ftruncate (o_krnl->loop_system, req, file, offset, cb);
}
int osal_fs_sendfile (uv_fs_t* req, uv_file out_fd, uv_file in_fd, int64_t in_offset, size_t length, uv_fs_cb cb) {
    return uv_fs_sendfile (o_krnl->loop_system, req, out_fd, in_fd, in_offset, length, cb);
}
int osal_fs_access (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb) {
    return uv_fs_access (o_krnl->loop_system, req, path, mode, cb);
}
int osal_fs_chmod (uv_fs_t* req, const char* path, int mode, uv_fs_cb cb) {
    return uv_fs_chmod (o_krnl->loop_system, req, path, mode, cb);
}
int osal_fs_utime (uv_fs_t* req, const char* path, double atime, double mtime, uv_fs_cb cb) {
    return uv_fs_utime (o_krnl->loop_system, req, path, atime, mtime, cb);
}
int osal_fs_futime (uv_fs_t* req, uv_file file, double atime, double mtime, uv_fs_cb cb) {
    return uv_fs_futime (o_krnl->loop_system, req, file, atime, mtime, cb);
}
int osal_fs_lstat (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_lstat (o_krnl->loop_system, req, path, cb);
}
int osal_fs_link (uv_fs_t* req, const char* path, const char* new_path, uv_fs_cb cb) {
    return uv_fs_link (o_krnl->loop_system, req, path, new_path, cb);
}
int osal_fs_symlink (uv_fs_t* req, const char* path, const char* new_path, int flags, uv_fs_cb cb) {
    return uv_fs_symlink (o_krnl->loop_system, req, path, new_path, flags, cb);
}
int osal_fs_readlink (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_readlink (o_krnl->loop_system, req, path, cb);
}
int osal_fs_realpath (uv_fs_t* req, const char* path, uv_fs_cb cb) {
    return uv_fs_realpath (o_krnl->loop_system, req, path, cb);
}
int osal_fs_fchmod (uv_fs_t* req, uv_file file, int mode, uv_fs_cb cb) {
    return uv_fs_fchmod (o_krnl->loop_system, req, file, mode, cb);
}
int osal_fs_chown (uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb) {
    return uv_fs_chown (o_krnl->loop_system, req, path, uid, gid, cb);
}
int osal_fs_fchown (uv_fs_t* req, uv_file file, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb) {
    return uv_fs_fchown (o_krnl->loop_system, req, file, uid, gid, cb);
}
int osal_fs_lchown (uv_fs_t* req, const char* path, uv_uid_t uid, uv_gid_t gid, uv_fs_cb cb) {
    return uv_fs_lchown (o_krnl->loop_system, req, path, uid, gid, cb);
}
int osal_fs_event_init (uv_fs_event_t* handle) {
    return uv_fs_event_init (o_krnl->loop_system, handle);
}
int osal_fs_poll_init (uv_fs_poll_t* handle) {
    return uv_fs_poll_init (o_krnl->loop_system, handle);
}
// Custom implementation
int osal_fs_fcreate (const char* path) {
#if defined (UCM_OS_WINDOWS)
	HANDLE hFile = CreateFileA((LPCSTR) path, GENERIC_WRITE,
                               FILE_SHARE_READ | FILE_SHARE_WRITE,
                               NULL,
                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
                               NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        return 1;
    CloseHandle(hFile);
#else
    int hFile = open (path, O_CREAT | O_WRONLY | O_TRUNC, 0664);
    if (!hFile)
        return 1;
    close (hFile);
#endif
    return 0;
}
