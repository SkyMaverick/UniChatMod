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

#ifndef STRICT_ASINC_API
    #if defined(ENABLE_CUSTOM_LIBS)
        #include "uv.h"
    #else
        #include <uv.h>
    #endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    // *********************************************************
    //      CORE STRUCTURES
    // *********************************************************

#include "uni/udefs.h" // preprocessor defines and includes. MUST BE FIRST !!!
#include "uni/umain.h" // base enums, structures and constants

#include "uni/usignals.h"  // signals
#include "uni/ucontacts.h" // contacts
#include "uni/uevents.h"   // events

    // *********************************************************
    //      PLUGINS FUNCTIONALITY API IMPLEMENTATION
    // *********************************************************

#include "uni/uplugin.h"    // plugins base
#include "uni/udatabase.h"  // database plugins functionality
#include "uni/uprotocols.h" // protocol plugins functionality
#include "uni/ugui.h"       // GUI plugin functionality

#ifndef STRICT_ASINC_API
    #include "uni/uuvapi.h" // libuv callbacks
#endif
#include "uni/uapi.h" // main API callbacks

    // *********************************************************
    //      MAIN APPLICATION API STRUCTURE
    // *********************************************************

    /*! API structure. Provide for all plugins */
    typedef struct _ucm_functions_s
    {
        ucm_api_t api;
#ifndef STRICT_ASINC_API
        ucm_uv_t uv;
#endif
    } ucm_functions_t;

#ifndef STRICT_ASINC_API
    #define UCM_UV(X) (X)->uv
#endif
#define UCM_OSAL(X) (X)->api.sys
#define UCM_APP(X) (X)->api.app

#define UCM_LOOP_SYSTEM(X) (uv_loop_t*)((X)->app.get_loop(CORE_LOOP_SYSTEM))
#define UCM_LOOP_NETWORK(X) (uv_loop_t*)((X)->app.get_loop(CORE_LOOP_NETWORK))

    typedef ucm_plugin_t* (*cb_init_plugin)(ucm_functions_t*);
    // *********************************************************
    //      START/STOP/INFO MODULE
    // *********************************************************

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
