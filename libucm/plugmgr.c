#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <wchar.h>

#include "ucm.h"
#include "core.h"
#include "defs.h"
#include "api.h"

typedef struct ucm_module_s {
    ucm_plugin_t* plugin;
    uintptr_t handle;
    struct ucm_module_s* next;
} ucm_module_t;

#define PLUGIN(X) X->plugin
#define NULL_REG(X) UniAPI->sys.zmemory ((X), sizeof(ucm_plugin_t*) * UCM_DEF_PLUG_COUNT + 1);

ucm_module_t modules = {
    .plugin = NULL,
    .handle = 0,
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

    uintptr_t handle = UniAPI->sys.dlopen (filename);
    if (!handle) {
        ucm_etrace ("%s: %s\n", filename, _("plugin don't load"));
        return module;
    }

    char* err = NULL;
    cb_init_plugin _pfunc = (cb_init_plugin) UniAPI->sys.dlsym(handle,"_init_plugin");
    if ( (err = UniAPI->sys.dlerror(handle)) == NULL ) {
        ucm_plugin_t*plug = _pfunc (UniAPI);
        if (plug) {
            if ( _plugin_verify (plug) == UCM_RET_SUCCESS ) {
                module = UniAPI->sys.zmalloc (sizeof(ucm_module_t));
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
    UniAPI->sys.dlclose (handle);
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

    size_t plugs_count = 0;
    char buffer [UCM_PATH_MAX];

    uintptr_t dir = UniAPI->sys.dir_open (plug_path);
    
    char* name_buf = NULL;
    int type = 0;
    while ( ( type = UniAPI->sys.dir_next(&name_buf, dir) ) > 0) {
        if ((type == UCM_TYPE_DIROBJ_FILE) &&
            (strstr (name_buf, ".so"))) //FIXME Only linux
        {
            snprintf(buffer, UCM_PATH_MAX, "%s/%s", plug_path, name_buf);
            tmp_module->next = _plugin_load(buffer);
            if ( tmp_module->next ) {
                tmp_module = tmp_module->next;
                plugs_count++;
            }
        }
    }
    ucm_trace ("%s: %zu\n",_("Plugins found"), plugs_count);
    UniAPI->sys.dir_close(dir);

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

        UniAPI->sys.dlclose(m_del->handle);
        UniAPI->sys.free (m_del);
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
