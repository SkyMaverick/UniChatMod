#include "app.h"

static inline UCM_RET
load_core_library(ucm_cargs_t* args, cb_evhook hook) {
#if defined(UCM_OS_POSIX) || defined(UCM_OS_WINEMULATOR)

    core_handle = dlopen(LIBCORE_NAME, RTLD_LAZY);
    if (core_handle) {
        core_load = dlsym(core_handle, UCM_LOAD_FUNC);
        core_exec = dlsym(core_handle, UCM_EXEC_FUNC);
        core_unload = dlsym(core_handle, UCM_UNLOAD_FUNC);
        core_info = dlsym(core_handle, UCM_INFO_FUNC);
#else
    #ifdef DEBUG
    fprintf(stdout, "%s: %s\n", "Library search path", args->path_lib_abs);
    #endif

    core_handle = LoadLibraryA(LIBCORE_NAME);
    if (core_handle != INVALID_HANDLE_VALUE) {
        core_load = (ucm_func_load)GetProcAddress(core_handle, UCM_LOAD_FUNC);
        core_exec = (ucm_func_exec)GetProcAddress(core_handle, UCM_EXEC_FUNC);
        core_unload = (ucm_func_unload)GetProcAddress(core_handle, UCM_UNLOAD_FUNC);
        core_info = (ucm_func_unload)GetProcAddress(core_handle, UCM_INFO_FUNC);
#endif
        if (core_load && core_exec && core_unload && core_info) {
                info = core_info();
                if (info) {
                    if ((info->api.vmajor >= LIBCORE_API_MAJVER) && 
                        (info->api.vminor >= LIBCORE_API_MINVER)) {

                        ucm_api = core_load (args);
                        if (ucm_api) {
                            core_exec (0, NULL); // TODO

                            ucm_api->app.mainloop_hook_attach (hook, NULL, UCM_SIG_LOAD_SUCCESS);
                            ucm_api->app.wait_exit();
                            ucm_api->app.mainloop_hook_detach (hook);
                            
                            core_unload(&ucm_api);
                            return UCM_RET_SUCCESS;
                        } else {
                            return UCM_RET_EXCEPTION;
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
