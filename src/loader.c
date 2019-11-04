#include "app.h"

static inline UCM_RET
load_core_library(ucm_cargs_t* args, cb_evhook hook) {
#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)

    core_handle = dlopen(LIBCORE_NAME, RTLD_LAZY);
    if (core_handle) {
        core_start = dlsym(core_handle, UCM_START_FUNC);
        core_stop = dlsym(core_handle, UCM_STOP_FUNC);
        core_info = dlsym(core_handle, UCM_INFO_FUNC);
#else
    #ifdef DEBUG
    fprintf(stdout, "%s: %s\n", "Library search path", args->path_lib_abs);
    #endif

    core_handle = LoadLibraryA(LIBCORE_NAME);
    if (core_handle != INVALID_HANDLE_VALUE) {
        core_start = (ucm_cstart_func)GetProcAddress(core_handle, UCM_START_FUNC);
        core_stop = (ucm_cstop_func)GetProcAddress(core_handle, UCM_STOP_FUNC);
        core_info = (ucm_cinfo_func)GetProcAddress(core_handle, UCM_INFO_FUNC);
#endif
        if (core_start && core_stop && core_info) {
            info = malloc(sizeof(ucm_plugin_info_t));
            if (info) {
                core_info(UCM_INFO_CORE, (void*)info, sizeof(ucm_plugin_info_t), args);
                if ((info->api.vmajor >= LIBCORE_API_MAJVER) &&
                    (info->api.vminor >= LIBCORE_API_MINVER)) {
                    ucm_api = core_start(args);
                    if (ucm_api) {
                        ucm_api->app.mainloop_hook_attach(hook, NULL, UCM_SIG_LOAD_SUCCESS);
                        ucm_api->app.wait_exit();

                        ucm_api->app.mainloop_hook_detach(hook);
                        return UCM_RET_SUCCESS;
                    } else {
                        fprintf(stderr, "%s\n", "Core API load FAIL");
                        return UCM_RET_NOTIMPLEMENT;
                    }
                }
            } else {
                // TODO
                return UCM_RET_EXCEPTION;
            }
        } else {
            fprintf(stderr, "%s\n", "Core information load FAIL");
            return UCM_RET_EMPTY;
        }
    } else {
        return UCM_RET_SYSTEM_DLERROR;
    }
}
