#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <string.h>
#include <wchar.h>

#include "ucm.h"
#include "config.h"

const ucm_functions_t* app;
static const ucm_plugin_t plugin;

typedef struct {
    uv_fs_t req;
    uv_fs_t write_req;

    size_t offset;
} logfile_t;

static logfile_t log;

static void
cb_logger_function (ucm_plugin_t*   plugin,
                    uint32_t        type,
                    const char*     txt,
                    void*           ctx)
{
    size_t len = strlen(txt);

    uv_buf_t buffer = app->uv.buf_init ((char*)txt, len);
    app->uv.fs_write (&(log.write_req), log.req.result, &buffer, 1, log.offset, NULL);

    log.offset += len;
}

static UCM_RET
_run_logger (void)
{
    char path [UCM_PATH_MAX];
    snprintf (path, UCM_PATH_MAX, "%s%c%s", app->app.get_startup_path(), PATH_DELIM, "ucm.log");
    log.offset = 0;

    int ret = app->uv.fs_open (&(log.req), path, UV_FS_O_RDWR | UV_FS_O_CREAT, 0666, NULL);
    if (ret)
        return UCM_RET_EXCEPTION;

    app->app.logger_connect (cb_logger_function, NULL);

    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_logger (void)
{
    app->app.logger_disconnect (cb_logger_function);

    uv_fs_t close_req;

#if defined (UCM_OS_WINDOWS)
    app->uv.fs_close (&close_req, log.req.file.fd, NULL);
#else
    app->uv.fs_close (&close_req, log.req.file, NULL);
#endif
    app->uv.fs_req_cleanup(&(log.req));
    app->uv.fs_req_cleanup(&(log.write_req));

    return UCM_RET_SUCCESS;
}

static void
_message (uint32_t id,
          uintptr_t ctx,
          uint32_t x1,
          uint32_t x2)
{
    //TODO
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

static const ucm_plugin_t plugin = {
    .oid                = UCM_TYPE_OBJECT_PLUGIN,
    .info.api           =
    {
        .vmajor         = UCM_API_MAJOR_VER,
        .vminor         = UCM_API_MINOR_VER
    },
    .info.sys           = UCM_TYPE_PLUG_STUFF,
    .info.vmajor        = UCM_VERSION_MAJOR,
    .info.vminor        = UCM_VERSION_MINOR,
    .info.vpatch        = UCM_VERSION_PATCH,
    .info.flags         = 0,
    .info.build         =
    {
        .commit         = UCM_BUILD_COMMIT,
        .datetime       = UCM_BUILD_TIME,
        .target         = UCM_BUILD_TARGET,
        .compiler       = UCM_BUILD_CC,
        .options        = UCM_BUILD_OPTS,
        .flags          = UCM_BUILD_FLAGS,
    },
    .info.pid           = "txt_log",
    .info.name          = L"Text logger",
    .info.developer     = L"SkyMaverick",
    .info.description   = L"Simple text file output logger",
    .info.copyright     = L"Zlib",
    .info.email         = L"mail@mail.ru",
    .info.website       = L"http://null.org",

    .run                = _run_logger,
    .stop               = _stop_logger,
    .message            = _message,
};

LIBUCM_API ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
