#include "ucm.h"
#include "db_mdbx.h"

#include "config.h"
#include "gettext.h"
#include "mdbx.h"
#include "mdbx_core.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

const ucm_functions_t* app;
static mdbx_database_t dba;

// ######################################################################
//      INTERNAL PLUGIN SERVICE FUNCTIONS
// ######################################################################

void
_assert_func(const MDBX_env* env, const char* msg, const char* function, unsigned line)
{
    UNUSED(env);
    trace_err(_("%s. Assert in %s (line: %d)\n"), msg, function, line);
}
// ######################################################################
//      STANDART PLUGIN API IMPLEMENTATION
// ######################################################################

static UCM_RET
_run_dbmdbx(void)
{
    return UCM_RET_SUCCESS;
}

static UCM_RET
_stop_dbmdbx(void)
{
    return UCM_RET_SUCCESS;
}

static void
_message(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2)
{
    // TODO
    UNUSED(id);
    UNUSED(ctx);
    UNUSED(x1);
    UNUSED(x2);
}

// ######################################################################
//      DATABASE PLUGIN API IMPLEMENTATION
// ######################################################################

static ucm_dbval_t*
get_provider(HCONTACT contact, ucm_object_t* object, const char* setting, ucm_dbval_t* defVal)
{
    UNUSED (contact);
    if (setting == NULL) {
        trace_dbg("%s\n", "Settings name must not empty");
        return NULL;
    }
    if (object) {
        switch (*object) {
        case UCM_TYPE_OBJECT_PLUGIN:
            break;
        case UCM_TYPE_OBJECT_EVENT:
            break;
        case UCM_TYPE_OBJECT_CONNECT:
            break;
        case UCM_TYPE_OBJECT_CONTACT:
        case UCM_TYPE_OBJECT_NULL:
        default:
            goto contact_settings;
        }
    } else {
    contact_settings:
        // TODO return contact setting
        return defVal;
    }
    return NULL;
}
static UCM_RET
set_provider(HCONTACT contact, ucm_object_t* object, const char* setting, ucm_dbval_t* value)
{
    UNUSED (contact);
    UNUSED (value);
    
    if (setting == NULL) {
        return UCM_RET_UNKNOWERROR;
    }
    if (object) {
        switch (*object) {
        case UCM_TYPE_OBJECT_PLUGIN:
            break;
        case UCM_TYPE_OBJECT_EVENT:
            break;
        case UCM_TYPE_OBJECT_CONNECT:
            break;
        case UCM_TYPE_OBJECT_CONTACT:
        case UCM_TYPE_OBJECT_NULL:
        default:
            goto contact_settings;
        }
    } else {
    contact_settings:
        return UCM_RET_SUCCESS;
    }
    return UCM_RET_SUCCESS;
}

static mdbx_database_t dba = {.plugin.core.info.api    = {.vmajor = UCM_API_MAJOR_VER, .vminor = UCM_API_MINOR_VER},
                              .plugin.core.info.sys    = UCM_TYPE_PLUG_DB,
                              .plugin.core.info.vmajor = UCM_VERSION_MAJOR,
                              .plugin.core.info.vminor = UCM_VERSION_MINOR,
                              .plugin.core.info.vpatch = UCM_VERSION_PATCH,
                              .plugin.core.info.flags  = UCM_FLAG_PLUG_LOGGED,
                              .plugin.core.info.build =
                                  {
                                      .commit   = UCM_BUILD_COMMIT,
                                      .datetime = UCM_BUILD_TIME,
                                      .target   = UCM_BUILD_TARGET,
                                      .compiler = UCM_BUILD_CC,
                                      .options  = UCM_BUILD_OPTS,
                                      .flags    = UCM_BUILD_FLAGS,
                                  },
                              .plugin.core.info.pid         = "f00e9e3d-0d5c-46e7-88b2-7d554ed2cb49",
                              .plugin.core.info.name        = L"std_dbmdbx",
                              .plugin.core.info.developer   = L"SkyMaverick",
                              .plugin.core.info.description = L"System standart storage plugin (based on libmdbx).",
                              .plugin.core.info.copyright   = L"Zlib + ReOpenLDAP",
                              .plugin.core.info.email       = L"mail@mail.ru",
                              .plugin.core.info.website     = L"http://null.org",

                              .plugin.core.run     = _run_dbmdbx,
                              .plugin.core.stop    = _stop_dbmdbx,
                              .plugin.core.message = _message,

                              .plugin.db_open  = mdbx_db_open,
                              .plugin.db_check = NULL,
                              .plugin.db_flush = mdbx_db_flush,
                              .plugin.db_close = mdbx_db_close,

                              .plugin.db_backup = mdbx_db_backup,
                              .plugin.get_setting = get_provider,
                              .plugin.set_setting = set_provider};

mdbx_database_t* UniDB = &dba;

LIBUCM_API ucm_plugin_t*
_init_plugin(const ucm_functions_t* api)
{
    app = api;
    return (ucm_plugin_t*)(&dba);
}
