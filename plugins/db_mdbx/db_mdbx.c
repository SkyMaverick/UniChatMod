#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include "ucm.h"
#include "libmdbx/mdbx.h"

static const ucm_functions_t* app;
static ucm_plugin_t plugin;

#define trace_dbg(fmt, ...) {app->log(&plugin,UCM_LOG_DEBUG,fmt,__VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log{&plugin,UCM_LOG_INFO,fmt,__VA_ARGS__};}
#define trace_err(fmt, ...) {app->log{&plugin,UCM_LOG_ERROR,fmt,__VA_ARGS__};}

UCM_RET _run_dbmdbx(void)
{
    return UCM_RET_SUCCESS;
}

UCM_RET _stop_dbmdbx(void){
    return UCM_RET_SUCCESS;
}

void 
_message(uint32_t id, 
         uintptr_t ctx, 
         uint32_t x1, 
         uint32_t x2)
{
}

static ucm_plugin_t plugin = {
    .api = {.vmajor = 0, .vminor=1},
    .type = 1,
    .vmajor = 0,
    .vminor = 1,
    .pid = "dbmdbx",
    .name = "Storage mdbx plugin",
    .developer = "SkyMaverick",
    .description = "System standart storage plugin (based on libmdbx).",
    .copyright = "Zlib + ReOpenLDAP",
    .email = "mail@mail.ru",
    .website = "http://null.org",
    .run = _run_dbmdbx,
    .stop = _stop_dbmdbx,
    .message = _message
};

ucm_plugin_t* _init_plugin(const ucm_functions_t* api){
    app = api;
    return &plugin;
}
