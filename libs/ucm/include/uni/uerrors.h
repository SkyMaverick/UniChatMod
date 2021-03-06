#pragma once

/*! Core return status */
typedef enum _ucm_status_return_e {
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

    UCM_RET_CORE_BADOPTS,
    UCM_RET_CORE_WRONGCTX,

    UCM_RET_UNKNOWERROR
} UCM_RET;

static const char* errors[UCM_RET_UNKNOWERROR + 1] = {
    //   UCM_RET_SUCCESS,
    _("Success return"),
    //   UCM_RET_WRONGPARAM,
    _("Function parameter wrong or empty"),
    //   UCM_RET_EXCEPTION,
    _("Unhandled internal exception"),
    //   UCM_RET_NOTIMPLEMENT,
    _("Required functionality not implement"),
    //   UCM_RET_NOOBJECT,
    _("Required object not found"),
    //   UCM_RET_EMPTY,
    _("Required object is empty"),
    //   UCM_RET_BUSY,
    _("Required resource not access"),
    //
    //   UCM_RET_DATABASE_UNKWNERROR,
    _("Unknown database error"),
    //   UCM_RET_DATABASE_BADFORMAT,
    _("Missing database format"),
    //   UCM_RET_DATABASE_BADINIT,
    _("Database initialization function not complete"),
    //   UCM_RET_DATABASE_BADINTERNAL,
    _("Database structure is corrupted and needs to be fixed"),
    //   UCM_RET_DATABASE_BADVERSION,
    _("Database version doesn't match supported and needs to convert"),
    //   UCM_RET_DATABASE_BADMETADATA,
    _("Database metadata is corrupted and needs to be fixed"),
    //   UCM_RET_DATABASE_NOTCHANGE,
    _("The operation of change to the database did not lead to success"),
    //
    //   UCM_RET_SYSTEM_NOCREATE,
    _("Don't create system object (mutex / file / socket etc.)"),
    //   UCM_RET_SYSTEM_NOACCESS,
    _("Don't access to system object"),
    //   UCM_RET_SYSTEM_NOMEMORY,
    _("Don't allocate memory"),
    //   UCM_RET_SYSTEM_DLERROR,
    _("Don't load shared object dependency"),
    //
    //   UCM_RET_MQUEUE_OVERFLOW,
    _("Application message queue is overflow"),
    //   UCM_RET_MQUEUE_EMPTY,
    _("Application message queue is empty"),
    //
    //   UCM_RET_PLUGIN_BADMODULE,
    _("This module NOT plugin"),
    //   UCM_RET_PLUGIN_BADVERSION,
    _("This plugin implements an unsupported version of the API"),
    //   UCM_RET_PLUGIN_BADSYSTEM,
    _("This plugin use wrong subsystem ID"),
    //   UCM_RET_PLUGIN_BADPID,
    _("This plugin use wrong PID"),
    //   UCM_RET_PLUGIN_BADIFACE,
    _("This plugin don't have core start/stop interfaces"),

    //   UCM_RET_CORE_BADOPTS
    _("This option not realized in UCM core"),
    //   UCM_RET_CORE_WRONGCTX
    _("UCM core detect wrong startup parameters"),

    //   UCM_RET_UNKNOWERROR
    _("Unknown error message")};

static const char*
ucm_strerr_func(const unsigned err) {
    fprintf(stdout, "%s: %d\n", "Catch number", err);
    return (err > UCM_RET_UNKNOWERROR) ? errors[UCM_RET_UNKNOWERROR] : errors[err];
}
