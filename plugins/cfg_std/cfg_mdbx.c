#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include "../../gcfm.h"

static const gc_functions_t* app;
static gc_plugin_t plugin;

#define trace_dbg(fmt, ...) {app->log(&plugin,GC_LOG_DEBUG,fmt,__VA_ARGS__);}
#define trace_inf(fmt, ...) {app->log{&plugin,GC_LOG_INFO,fmt,__VA_ARGS__};}
#define trace_err(fmt, ...) {app->log{&plugin,GC_LOG_ERROR,fmt,__VA_ARGS__};}

GC_RET _run_demo(void){
//    trace_dbg("%s\n","Run procedure in demo.c");
    return RET_SUCCESS;
}

GC_RET _stop_demo(void){
//    trace_dbg("%s\n","Stop procedure in demo.c");
    return RET_SUCCESS;
}

void _message(uint32_t id, uintptr_t ctx, 
                            uint32_t x1, uint32_t x2){
}

static gc_plugin_t plugin = {
    .api = {.vmajor = 0, .vminor=1},
    .type = PL_EXT,
    .vmajor = 0,
    .vminor = 1,
    .pid = "demo",
    .name = "Demo plugin",
    .developer = "SkyMaverick",
    .description = "Demo plugin for demonstrate realisation and test functionality",
    .copyright = "Zlib",
    .email = "mail@mail.ru",
    .website = "http://null.org",
    .run = _run_demo,
    .stop = _stop_demo,
    .message = _message
};

gc_plugin_t* _init_plugin(const gc_functions_t* api){
    app = api;
    return &plugin;
}
