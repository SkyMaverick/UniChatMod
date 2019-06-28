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

#if defined (ENABLE_CUSTOM_LIBS)
    #include "uv.h"
#else
    #include <uv.h>
#endif

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
static uintptr_t _lock = 0;

static size_t plugins_limit        = UCM_DEF_PLUG_COUNT;
static size_t plugins_all_count        = 0;
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
        return UCM_RET_PLUGIN_BADMODULE;
    }

    if (plugin->info.api.vmajor != UCM_API_MAJOR_VER) {
        return UCM_RET_PLUGIN_BADVERSION;
    }
    if (plugin->info.sys > UCM_TYPE_PLUG_STUFF) {
        return UCM_RET_PLUGIN_BADSYSTEM;
    }
    // TODO strongly PID validation
    if (plugin->info.pid) {
        if ( strcmp (plugin->info.pid, "") == 0 ) {
            return UCM_RET_PLUGIN_BADPID;
        } 
        if ( strlen(plugin->info.pid) > UCM_PID_MAX ) {
            return UCM_RET_PLUGIN_BADPID;;
        }
    }
    if (( plugin->run  == NULL ) ||
        ( plugin->stop == NULL )) {
        return UCM_RET_PLUGIN_BADIFACE;
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

    cb_init_plugin _pfunc = (cb_init_plugin) UniAPI->sys.dlsym(handle,"_init_plugin");
    if ( _pfunc ) {
        ucm_plugin_t*plug = _pfunc (UniAPI);
        if (plug) {
            int ret_code = _plugin_verify (plug);
            if (  ret_code == UCM_RET_SUCCESS ) {
                module = UniAPI->sys.zmalloc (sizeof(ucm_module_t));
                if (module) {
                    module->plugin = plug;
                    module->handle = handle;
                    return module;
                }
           } else {
               ucm_etrace ("%s. %s\n", filename, ucm_strerr(ret_code));
           }
        } else {
            ucm_etrace ("%s: %s\n", filename, _("this plugin broken initialization"));
            ucm_dtrace ("%s: %s\n", "Library load error", UniAPI->sys.dlerror(handle));
        }
    } else {
        ucm_etrace ("%s - %s\n", filename, _("this library isn't plugin"));
    }
    UniAPI->sys.dlclose (handle);
    return module;
}

static inline void
_plugin_registry_add (ucm_plugin_t* plugin)
{
                plugins_all[ plugins_all_count ] = plugin;

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
        if ( plugins_limit > plugins_all_count ) {
            if ( m_tmp->plugin->run() == UCM_RET_SUCCESS ) {
                _plugin_registry_add(m_tmp->plugin);
                plugins_all_count += 1;
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

static size_t
__loaddir_cb (uv_fs_t* req)
{
    uv_dirent_t dent;
    uv_fs_t     close_req;
    char buffer [UCM_PATH_MAX];

    size_t count = 0;

    ucm_dtrace("%s ...\n", "Scan plugin directory");

    while ( UniAPI->uv.fs_scandir_next(req, &dent) != UV__EOF) {
        ucm_dtrace("%s%c%s\n", req->path, PATH_DELIM, dent.name);
        
        snprintf (buffer, UCM_PATH_MAX, "%s%c%s", req->path, PATH_DELIM, dent.name);

        ucm_module_t* tmp = _plugin_load(buffer);
        if (tmp) {
            // update modules list
            UniAPI->sys.rwlock_wlock(_lock);

            tmp->next = modules.next;
            modules.next = tmp;

            count ++;

            UniAPI->sys.rwlock_unlock(_lock);
        }
    }
#if defined (UCM_OS_WINDOWS) 
    UniAPI->uv.fs_close (&close_req, req->file.fd, NULL);
#else
    UniAPI->uv.fs_close (&close_req, req->file, NULL);
#endif
    UniAPI->uv.fs_req_cleanup(&close_req);

    return count;
}

size_t
plugins_load_registry (const char* plug_path)
{
    uv_fs_t req;
    size_t ret = 0;

    _lock = UniAPI->sys.rwlock_create();
    int r = UniAPI->uv.fs_scandir (&req, plug_path, O_RDONLY, NULL);
    if (r < 0) {
        ucm_dtrace ("%s: %s\n", "Scandir error", UniAPI->uv.strerror(r));
        return 0;
    }
    __loaddir_cb (&req);
    UniAPI->uv.fs_req_cleanup(&req);

    ucm_dtrace ("%s: %zu\n", "Found plugins count", plugins_all_count);
    return ret;
}

void
plugins_release_registry (void)
{
    ucm_module_t* m_tmp = modules.next;
    ucm_module_t* m_del = NULL;

    while(m_tmp) {
        m_del = m_tmp;
        m_tmp = m_tmp->next;
#ifndef ENABLE_VALGRIND
        UniAPI->sys.dlclose(m_del->handle);
#else
        UniAPI->sys.free((void*)m_del->handle);
#endif
        UniAPI->sys.free (m_del);
    }
    UniAPI->sys.rwlock_free(_lock);
}

void
plugins_message_dispatch (const uint32_t* id,
                          const uintptr_t* ctx,
                          const uint32_t* x1,
                          const uint32_t* x2)
{
    if (modules.plugin)
        modules.plugin->message(*id, *ctx, *x1, *x2);   // core receive message first

    for ( size_t i = 0; i < plugins_all_count; i++ ) {
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

const size_t
plugins_count (void) {
    return plugins_all_count;
}
