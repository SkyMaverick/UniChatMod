#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>
#include <stdint.h>
#include <wchar.h>

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
#define NULL_REG(X) ucm_kzmemory ((X),   sizeof(ucm_plugin_t*) * UCM_DEF_PLUG_COUNT + 1);

ucm_module_t modules = {
    .plugin = NULL,
    .handle = NULL,
    .next   = NULL
};

static size_t plugins_limit        = UCM_DEF_PLUG_COUNT;
static size_t plugins_count        = 0;
static ucm_plugin_t* plugins_all   [UCM_DEF_PLUG_COUNT + 1];
static size_t plugins_db_count     = 0;
static ucm_plugin_t* plugins_db    [UCM_DEF_PLUG_COUNT + 1];
static size_t plugins_proto_count  = 0;
static ucm_plugin_t* plugins_proto [UCM_DEF_PLUG_COUNT + 1];
static size_t plugins_cript_count  = 0;
static ucm_plugin_t* plugins_crypt [UCM_DEF_PLUG_COUNT + 1];
static size_t plugins_hist_count   = 0;
static ucm_plugin_t* plugins_hist  [UCM_DEF_PLUG_COUNT + 1];
static size_t plugins_gui_count    = 0;
static ucm_plugin_t* plugins_gui   [UCM_DEF_PLUG_COUNT + 1];
static size_t plugins_stuff_count  = 0;
static ucm_plugin_t* plugins_stuff [UCM_DEF_PLUG_COUNT + 1];

// ######################################################################
//      PRIVATE API IMPLEMENTATION
// ######################################################################

static UCM_RET
_plugin_verify (ucm_plugin_t* plugin)
{
    if (plugin->oid != UCM_TYPE_OBJECT_PLUGIN) {
        return UCM_RET_INVALID;
    }

    if (plugin->info.api.vmajor != UCM_API_MAJOR_VER) {
        return UCM_RET_UNREALIZED;
    }
    if (plugin->info.sys > UCM_TYPE_PLUG_STUFF) {
        return UCM_RET_UBOUND;
    }
    // TODO strongly PID validation
    if (plugin->info.pid) {
        if ( wcscmp (plugin->info.pid, L"") == 0 ) {
            return UCM_RET_NOOBJECT;
        }
    }
    if (( plugin->run  == NULL ) ||
        ( plugin->stop == NULL )) {
        return UCM_RET_UNREALIZED;
    }
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
        ucm_plugin_t* plug = cb_init_plugin(ucm_api);
        if (plug) {
            if ( _plugin_verify (plug) == UCM_RET_SUCCESS ) {
                module = ucm_kmalloc (sizeof(ucm_module_t));
                if (module) {
                    module->plugin = plug;
                    module->handle = handle;
                    return module;
                }
           } else {
               ucm_etrace ("%s: %s\n", filename, _("this plugin broken"));
           }
        } else {
            ucm_etrace ("%s: %s\n", filename, _("this plugin broken initialization"));
        }
    }
    dlclose (handle);
    return module;
}

static inline void
_plugin_registry_add (ucm_plugin_t* plugin)
{
                plugins_all[ plugins_count ] = plugin;

                switch (plugin->info.sys) {
                    case UCM_TYPE_PLUG_DB:
                        {
                            plugins_db [plugins_db_count++] = plugin;
                            break;
                        }
                    case UCM_TYPE_PLUG_PROTO:
                        {
                            plugins_proto [plugins_proto_count++] = plugin;
                            break;
                        }
                    case UCM_TYPE_PLUG_CRYPTO:
                        {
                            plugins_crypt [plugins_cript_count++] = plugin;
                            break;
                        }
                    case UCM_TYPE_PLUG_HIST:
                        {
                            plugins_hist [plugins_hist_count++] = plugin;
                            break;
                        }
                    case UCM_TYPE_PLUG_GUI:
                        {
                            plugins_gui [plugins_gui_count++] = plugin;
                            break;
                        }
                    case UCM_TYPE_PLUG_STUFF:
                        {
                            plugins_stuff [plugins_stuff_count++] = plugin;
                            break;
                        }
                }
}

// ######################################################################
//      PUBLIC API IMPLEMENTATION
// ######################################################################

void
plugins_run_all (void)
{
    NULL_REG (plugins_all);
    NULL_REG (plugins_db);
    NULL_REG (plugins_proto);
    NULL_REG (plugins_crypt);
    NULL_REG (plugins_hist);
    NULL_REG (plugins_stuff);

    ucm_module_t* m_tmp = modules.next;

    for ( ; m_tmp; m_tmp = m_tmp->next) {
        if ( plugins_limit > plugins_count ) {
            if ( m_tmp->plugin->run() == UCM_RET_SUCCESS ) {
                _plugin_registry_add(m_tmp->plugin);
                plugins_count += 1;
            } else {
                ucm_etrace("%ls: %s\n", m_tmp->plugin->info.pid,
                     _("plugin start missing. Ignore this plugin."));
            }
        }
    }
}

void
plugins_stop_all (void)
{
    for (size_t i = 0; i < UCM_DEF_PLUG_COUNT; i++) {
        if (plugins_all[i] != NULL) {
            plugins_all[i]->stop();
        } else {
            break;
        }
    }
    NULL_REG (plugins_all);
    NULL_REG (plugins_db);
    NULL_REG (plugins_proto);
    NULL_REG (plugins_crypt);
    NULL_REG (plugins_hist);
    NULL_REG (plugins_stuff);
}

UCM_RET
plugins_load_registry (const char* plug_path)
{
    modules.plugin = ucm_core;
    ucm_module_t* tmp_module = &modules;

    DIR* plugs_dir = opendir(plug_path);
    if (!plugs_dir) {
        ucm_etrace("%ls %ls: %ls\n", _("Not found path"), plug_path, strerror(errno));
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
        if ( tmp_module->next ) {
            tmp_module = tmp_module->next;
            plugs_count++;
        }
    }

    ucm_trace ("%s: %zu\n",_("Plugins found"), plugs_count);
    closedir(plugs_dir);
    return UCM_RET_SUCCESS;
}

void
plugins_release_registry (void)
{
    ucm_module_t* m_tmp = modules.next;
    ucm_module_t* m_del = NULL;

    while(m_tmp) {
        m_del = m_tmp;
        m_tmp = m_tmp->next;

        dlclose(m_del->handle);
        ucm_kfree(m_del);
    }
}

void
plugins_message_dispatch (const uint32_t* id,
                          const uintptr_t* ctx,
                          const uint32_t* x1,
                          const uint32_t* x2)
{
    modules.plugin->message(*id, *ctx, *x1, *x2);   // core receive message first

    for ( size_t i = 0; i < plugins_count; i++ ) {
        if (plugins_all[i] && plugins_all[i]->message) {
            plugins_all[i]->message (*id, *ctx, *x1, *x2);
        }
    }
}

const ucm_plugin_t**
plugins_get_all (void)
{
    return (const ucm_plugin_t**) plugins_all;
}

const ucm_plugin_t**
plugins_get_db (void)
{
    return (const ucm_plugin_t**) plugins_db;
}

const ucm_plugin_t**
plugins_get_proto (void)
{
    return (const ucm_plugin_t**) plugins_proto;
}

const ucm_plugin_t**
plugins_get_crypt (void)
{
    return (const ucm_plugin_t**) plugins_crypt;
}

const ucm_plugin_t**
plugins_get_hist (void)
{
    return (const ucm_plugin_t**) plugins_hist;
}

const ucm_plugin_t**
plugins_get_gui (void)
{
    return (const ucm_plugin_t**) plugins_gui;
}

const ucm_plugin_t**
plugins_get_stuff (void)
{
    return (const ucm_plugin_t**) plugins_stuff;
}
