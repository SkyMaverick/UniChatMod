#include "ucm.h"

#include "api.h"
#include "core.h"
#include "defs.h"
#include "logger.h"
#include "plugmgr.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <flags.h>

LIBUCM_API ucm_functions_t*
ucm_core_load(ucm_cargs_t* args) {
    return NULL;
}

LIBUCM_API UCM_RET
ucm_core_exec(uint32_t flags, void* ctx) {
    return UCM_RET_SUCCESS;
}

LIBUCM_API UCM_RET
ucm_core_unload(ucm_functions_t** api) {
    return UCM_RET_SUCCESS;
}

LIBUCM_API const ucm_plugin_info_t*
ucm_core_info (void)
{
    return &(ucm_core->info);
}


// static void
// _prepare_opts(const ucm_cargs_t* args) {
//     uint64_t opts = args->options;
//
//     if (opts & UCM_FLAG_MODE_READONLY)
//         set_system_flag(UCM_FLAG_ROPROF);
//
//     // TODO Translate application mode flags in UCM flags here
//
//     if (opts & UCM_FLAG_MODE_NEWPROFILE) {
//         set_system_flag(UCM_FLAG_NEWPROF);
//         unset_system_flag(UCM_FLAG_ROPROF);
//     };
// }
//
// static int
// _prepare_args(const ucm_cargs_t* args) {
//     extern char ucm_path[UCM_PATH_MAX];
//     extern char ucm_path_store[UCM_PATH_MAX];
//     extern char ucm_path_plugs[UCM_PATH_MAX];
//
//     if (args->path_abs && args->path_plug_abs && args->path_store_abs) {
//         snprintf(ucm_path, UCM_PATH_MAX, "%s", args->path_abs);
//         snprintf(ucm_path_store, UCM_PATH_MAX, "%s", args->path_store_abs);
//         snprintf(ucm_path_plugs, UCM_PATH_MAX, "%s", args->path_plug_abs);
//
//         _prepare_opts(args);
//     } else {
//         return 0;
//     }
//     return 1;
// }
//
// LIBUCM_API ucm_functions_t*
// ucm_core_start(ucm_cargs_t* args) {
//     if (!_prepare_args(args))
//         return NULL;
//
//     int ret_code = core_load();
//     if (ret_code != UCM_RET_SUCCESS) {
//         ucm_etrace("%s\n", ucm_strerr(ret_code));
//         return NULL;
//     };
//
//     return UniAPI;
// }
//
// LIBUCM_API UCM_RET
// ucm_core_stop(void) {
//     int ret_code = core_unload();
//     return ret_code;
// }
//
// /* This functions provide information for external applications */
//
// static size_t
// handle_info_core(void* mem, size_t mem_size, ucm_cargs_t* args) {
//     UNUSED(args);
//     size_t needed = sizeof(ucm_plugin_info_t);
//     if (mem) {
//         if (mem_size < needed)
//             needed = mem_size;
//         memcpy(mem, &(ucm_core->info), needed);
//     }
//     return needed;
// }
//
// static size_t
// handle_info_plug(void* mem, size_t mem_size, ucm_cargs_t* args) {
//     UNUSED(mem);
//     UNUSED(mem_size);
//     UNUSED(args);
//     //    compat_layer_init ();
//     //    size_t count = plugins_load_registry (UniAPI->app.get_plugins_path());
//     //
//     //    if (count > 0) {
//     //
//     //        ucm_plugin_info_t* inf_arr;
//     //        inf_arr = UniAPI->sys.zmalloc (sizeof(ucm_plugin_info_t) * count);
//     //        if (inf_arr == NULL)
//     //            goto bailout;
//     //
//     //        ucm_plugin_t** tmp = (ucm_plugin_t**)
//     //        UniAPI->app.get_plugins_all(); for ( size_t i = 0; *tmp; tmp++,
//     //        i++ )
//     //            memcpy (&(inf_arr[i]), &((*tmp)->info),
//     //            sizeof(ucm_plugin_info_t));
//     //
//     //        *info = (void*) inf_arr;
//     //    }
//     // bailout: plugins_release_registry();
//     //    compat_layer_release ();
//     //    return count;
//     //
//     return 0;
// }
//
// LIBUCM_API size_t
// ucm_core_info(uint8_t mode, void* mem, size_t mem_size, ucm_cargs_t* args) {
//     if (!_prepare_args(args))
//         goto bailout;
//
//     switch (mode) {
//     case UCM_INFO_CORE:
//         return handle_info_core(mem, mem_size, args);
//     case UCM_INFO_PLUGINS:
//         return handle_info_plug(mem, mem_size, args);
//     }
// bailout:
//     return 0;
// }
