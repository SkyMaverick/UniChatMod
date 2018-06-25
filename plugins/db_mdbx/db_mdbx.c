#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ucm.h"
#include "config.h"
#include "gettext.h"
#include "libmdbx/mdbx.h"

static const ucm_functions_t* app;
static ucm_dbplugin_t plugin;

typedef struct {
    uint8_t         type;
    union {
        uint8_t     byte;
        uint16_t    word;
        uint32_t    d_word;
        uint64_t    q_word;
        char*       string;
        struct {
            size_t  lenght;
            void*   data;
        } blob;
    };
} db_value_t;

typedef struct {
    uint32_t    signature;
    uint32_t    version;
} db_header_t;

typedef struct {

    char            file_apath [PATH_MAX];

    MDBX_env*       env;
    MDBX_txn*       txn;

    MDBX_dbi        dbi_global;
    MDBX_cursor*    cur_global;

    MDBX_dbi        dbi_events;
    MDBX_cursor*    cur_events;

    MDBX_dbi        dbi_contacts;
    MDBX_cursor*    cur_contacts;

    MDBX_dbi        dbi_plugins;
    MDBX_cursor*    cur_plugins;
} mdbx_dbmgr_t;

#define trace_dbg(fmt, ...) {app->log ( (ucm_plugin_t*)(&plugin), UCM_LOG_DEBUG, fmt, __VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log ( (ucm_plugin_t*)(&plugin), UCM_LOG_INFO,  fmt, __VA_ARGS__);}
#define trace_err(fmt, ...) {app->log ( (ucm_plugin_t*)(&plugin), UCM_LOG_ERROR, fmt, __VA_ARGS__);}

#define zmalloc(size) calloc(1,size)

static mdbx_dbmgr_t* UCM_MDBX_DB = NULL;

// ######################################################################
//      INTERNAL PLUGIN SERVICE FUNCTIONS
// ######################################################################

static inline MDBX_txn*
_start_mdbx_txn (uint32_t flags)
{
    MDBX_txn* result = NULL;
    int rc = mdbx_txn_begin (UCM_MDBX_DB->env,
                             NULL,
                             (flags | UCM_FLAG_DB_READONLY) ? MDBX_RDONLY : 0,
                             &result);
    return (rc != MDBX_SUCCESS) ? NULL : result;
}

// static void*
// _touch_mdbx_file (char* fname)
// {
//     void* handle =
//     return
// }
//
// ######################################################################
//      STANDART PLUGIN API IMPLEMENTATION
// ######################################################################

static UCM_RET
_run_dbmdbx (void)
{
    int rc;
    UCM_MDBX_DB = zmalloc(sizeof(mdbx_dbmgr_t));
    if (UCM_MDBX_DB) {
          rc = mdbx_env_create(&(UCM_MDBX_DB->env));
          if (rc != MDBX_SUCCESS) {
              trace_err("%s: (%d) %s\n", _("mdbx_env_create:"), rc, mdbx_strerror(rc));
          } else {
              trace_dbg("%s\n", "Create mdbx ENV success");
              return UCM_RET_SUCCESS;
          }
    }
    return UCM_RET_NONALLOC;
}

static UCM_RET
_stop_dbmdbx (void)
{
    plugin.db_close();
    free (UCM_MDBX_DB);
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id,
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

// ######################################################################
//      DATABASE PLUGIN API IMPLEMENTATION
// ######################################################################

static UCM_RET
mdbx_db_open  (char*    file,
               uint32_t flags)
{
    UNUSED(flags);
    snprintf (UCM_MDBX_DB->file_apath, PATH_MAX, "%s", file);

    return UCM_RET_SUCCESS;
}

static UCM_RET
mdbx_db_close (void)
{
    // TODO
    return UCM_RET_SUCCESS;
}

static ucm_dbplugin_t plugin = {
    .core.info.api           =
    {
        .vmajor              = UCM_API_MAJOR_VER,
        .vminor              = UCM_API_MINOR_VER
    },
    .core.info.sys           = UCM_PLUG_DB,
    .core.info.vmajor        = UCM_VERSION_MAJOR,
    .core.info.vminor        = UCM_VERSION_MINOR,
    .core.info.vpatch        = UCM_VERSION_PATCH,
    .core.info.build         =
    {
        .commit              = UCM_BUILD_COMMIT,
        .datetime            = UCM_BUILD_TIME,
        .target              = UCM_BUILD_TARGET,
        .compiler            = UCM_BUILD_CC,
        .options             = UCM_BUILD_OPTS,
        .flags               = UCM_BUILD_FLAGS,
    },
    .core.info.pid           = "dbmdbx",
    .core.info.name          = "Storage mdbx plugin",
    .core.info.developer     = "SkyMaverick",
    .core.info.description   = "System standart storage plugin (based on libmdbx).",
    .core.info.copyright     = "Zlib + ReOpenLDAP",
    .core.info.email         = "mail@mail.ru",
    .core.info.website       = "http://null.org",

    .core.run                = _run_dbmdbx,
    .core.stop               = _stop_dbmdbx,
    .core.message            = _message,

    .db_open                 = mdbx_db_open,
    .db_check                = NULL,
    .db_flush                = NULL,
    .db_close                = mdbx_db_close,

    .get_int                 = NULL,
    .get_int64               = NULL,
    .get_float               = NULL,
    .get_str                 = NULL,
    .set_int                 = NULL,
    .set_int64               = NULL,
    .set_float               = NULL,
    .set_str                 = NULL,
    .item_del                = NULL
};

ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return (ucm_plugin_t*)(&plugin);
}
