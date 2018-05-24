#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>
#include <stdint.h>

#include "ucm.h"
#include "core.h"
#include "defs.h"
#include "api.h"

typedef struct ucm_module_s {
    ucm_plugin_t* plugin;
    void* handle;
    struct ucm_module_s* next;
} ucm_module_t;

#define PLUGIN(X) X->plugin

ucm_module_t modules = {
    .plugin = NULL,
    .handle = NULL,
    .next   = NULL
};

static UCM_RET
_plugin_verify (ucm_plugin_t* plugin)
{
    return UCM_RET_SUCCESS;
}

static ucm_module_t*
_plugin_load (char* filename)
{
    ucm_module_t* module = NULL;

    void* handle = dlopen (filename, RTLD_LAZY);
    if (!handle) {
        ucm_etrace ("%s: %s\n", filename, _("plugin don't load"));
        return module;
    }

    char* err = NULL;
    ucm_plugin_t* (*cb_init_plugin)(ucm_functions_t* api) = dlsym(handle,"_init_plugin");
    if ( (err = dlerror()) == NULL ) {
        ucm_plugin_t* plug = cb_init_plugin(ucm_global_api);
        if (plug) {
            if ( _plugin_verify (plug) == UCM_RET_SUCCESS )
                module = malloc (sizeof(ucm_module_t));
                if (module) {
                    module->plugin = plug;
                    module->handle = handle;
                }
       } else {
           ucm_etrace ("%s: %s", filename, _("this plugin broken"));
       }
    } else {
        ucm_etrace ("%s: %s", filename, _("this plugin broken initialization"));
    }

    dlclose (handle);
    return module;
}


UCM_RET
plugins_load_registry (char* plug_path)
{
    modules.plugin = ucm_global_core;
    ucm_module_t* tmp_module = &modules;

    DIR* plugs_dir = opendir(plug_path);
    if (!plugs_dir) {
        ucm_etrace("%s %s: %s\n", _("Not found path"), plug_path, strerror(errno));
        return UCM_RET_WRONGPARAM;
    }

    /* scan directory was marked as plugins container */
    struct dirent* ls = NULL;
    size_t plugs_count = 0;
    char buffer [UCM_PATH_MAX];

    while ((ls = readdir(plugs_dir))) {
        if (!(strncmp (ls->d_name, ".", 1))  ||
            !(strncmp (ls->d_name, "..", 2)) ||
            !(strstr (ls->d_name, ".so")))
        {
            continue;
        }
        snprintf(buffer, UCM_PATH_MAX, "%s/%s", plug_path, ls->d_name);
        tmp_module->next = _plugin_load(buffer);
        if ( tmp_module->next )
            tmp_module = tmp_module->next;
        plugs_count++;
    }

    ucm_trace ("%s: %zu\n",_("Plugins found"), plugs_count);
    closedir(plugs_dir);
    return UCM_RET_SUCCESS;
}

void
plugins_release_registry (void)
{
    ucm_module_t* tmp_module = modules.next;
    modules.next = NULL;

    while(tmp_module) {
        ucm_module_t* del_module = tmp_module;
        tmp_module = tmp_module->next;

        dlclose(del_module->handle);
        free(del_module);
    }
}
