#include "plugmgr.h"

#include "api.h"
#include "core.h"
#include "defs.h"
#include "ucm.h"

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

typedef struct ucm_module_s
{
    ucm_plugin_t* plugin; /* plugin handler */
    uintptr_t handle;     /* library descriptor */

    struct ucm_module_s* next; /* linked list element */
} ucm_module_t;

typedef struct
{
    // modules chain. Base structure which contain all load modules
    ucm_module_t* m_list;
    size_t found;
    // RWL-mutex for block update store
    uintptr_t lock;
    // flags
    uint32_t flags;

    struct
    {
        // All plugins count
        size_t count;
        // index last element for each type array
        size_t idx[UCM_TYPE_PLUG_STUFF + 1];

        /* Store for plugins pointer (format)

                                          idx [type]
                                           |
             [          0          ] [p1* ... pn*, <NULL>]
             [ UCM_TYPE_PLUG_DB    ] [p1* ... pn*, <NULL>]
                                ... *** ...
             [ UCM_TYPE_PLUG_STUFF ] [p1* ... pn*, <NULL>]

         */
        ucm_plugin_t* items[UCM_TYPE_PLUG_STUFF + 1][UCM_DEF_PLUG_COUNT + 1];
    } registry;
} ucm_pmgr_t;

static ucm_pmgr_t* UniPMgr = NULL;
#define U__REG(X, Y) UniPMgr->registry.items[(X)][(Y)]
#define U__IDX(X) UniPMgr->registry.idx[(X)]

/***************************************************
    INTERNAL API
 ***************************************************/

static UCM_RET
plugin_verify(ucm_plugin_t* plugin)
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
        if (strcmp(plugin->info.pid, "") == 0) {
            return UCM_RET_PLUGIN_BADPID;
        }
        if (strlen(plugin->info.pid) > UCM_PID_MAX) {
            return UCM_RET_PLUGIN_BADPID;
        }
    }
    if ((plugin->run == NULL) || (plugin->stop == NULL)) {
        return UCM_RET_PLUGIN_BADIFACE;
    }
    return UCM_RET_SUCCESS;
}

static ucm_module_t*
plugin_load(char* filename)
{
    ucm_module_t* module = NULL;

    // 1. Load library witch plugin potential
    uintptr_t handle = UniAPI->sys.dlopen(filename);
    if (!handle) {
        ucm_etrace("%s: %s\n", filename, _("plugin don't load"));
        return module;
    }

    // 2. Run INIT function. Provide core API or get plugin handle
    cb_init_plugin _pfunc = (cb_init_plugin)UniAPI->sys.dlsym(handle, "_init_plugin");
    if (_pfunc) {
        ucm_plugin_t* plug = _pfunc(UniAPI);
        if (plug) {
            // 3. Verfify plugin handle (check needed function)
            int ret_code = plugin_verify(plug);
            if (ret_code == UCM_RET_SUCCESS) {
                // 4. Create module object
                module = UniAPI->sys.zmalloc(sizeof(ucm_module_t));
                if (module) {
                    module->plugin = plug;
                    module->handle = handle;
                    return module;
                }
            } else {
                ucm_etrace("%s. %s\n", filename, UniAPI->sys.strerr(ret_code));
            }
        } else {
            ucm_etrace("%s: %s\n", filename, _("this plugin broken initialization"));
            ucm_dtrace("%s: %s\n", "Library load error", UniAPI->sys.dlerror(handle));
        }
    } else {
        ucm_etrace("%s - %s\n", filename, _("this library isn't plugin"));
    }
    UniAPI->sys.dlclose(handle);
    return module;
}

static void
scan_result_process(uv_fs_t* req)
{
    uv_dirent_t dent;
    uv_fs_t close_req;
    char buffer[UCM_PATH_MAX];

    ucm_dtrace("%s ...\n", "Scan plugin directory");

    while (UniAPI->uv.fs_scandir_next(req, &dent) != UV__EOF) {
        ucm_dtrace("%s%c%s\n", req->path, PATH_DELIM, dent.name);

        snprintf(buffer, UCM_PATH_MAX, "%s%c%s", req->path, PATH_DELIM, dent.name);

        ucm_module_t* tmp = plugin_load(buffer);
        if (tmp) {
            UniAPI->sys.rwlock_wlock(UniPMgr->lock);

            // update modules list
            tmp->next             = UniPMgr->m_list->next;
            UniPMgr->m_list->next = tmp;

            UniPMgr->found++;

            UniAPI->sys.rwlock_unlock(UniPMgr->lock);
        }
    }
#if defined(UCM_OS_WINDOWS)
    UniAPI->uv.fs_close(UCM_LOOP_SYSTEM(UniAPI), &close_req, req->file.fd, NULL);
#else
    UniAPI->uv.fs_close(UCM_LOOP_SYSTEM(UniAPI), &close_req, req->file, NULL);
#endif
    UniAPI->uv.fs_req_cleanup(&close_req);
}

/***************************************************
    EXTERNAL API
 ***************************************************/
size_t
pmgr_load(char* path, uint32_t flags)
{
    if (UniPMgr != NULL)
        return UniPMgr->registry.count;

    uv_fs_t req;
    // 1. Create manager object
    UniPMgr = UniAPI->sys.zmalloc(sizeof(ucm_pmgr_t));
    if (UniPMgr) {
        UniPMgr->flags = flags;
        // 2. Create modules list head element
        UniPMgr->m_list = UniAPI->sys.zmalloc(sizeof(ucm_module_t));
        if (UniPMgr->m_list) {
            // 3. Let head element is root core plugin
            UniPMgr->m_list->plugin = ucm_core;
            // 4. Create global mutex
            UniPMgr->lock = UniAPI->sys.rwlock_create();
            if (UniPMgr->lock) {
                // 5. Scan plugins directory and process it
                int r = UniAPI->uv.fs_scandir(UCM_LOOP_SYSTEM(UniAPI), &req, path, O_RDONLY, NULL);
                if (r >= 0) {
                    scan_result_process(&req);
                    UniAPI->uv.fs_req_cleanup(&req);

                    UniAPI->app.mainloop_msg_send(UCM_EVENT_PLUGS_SUCCESS, 0, UniPMgr->found, 0);

                    return UniPMgr->found;
                }
                UniAPI->uv.fs_req_cleanup(&req);
                ucm_dtrace("%s: %s\n", "Scandir error", UniAPI->uv.strerror(r));

                UniAPI->sys.rwlock_free(UniPMgr->lock);
            }
            ucm_free_null(UniPMgr->m_list);
        }
        ucm_free_null(UniPMgr);
    }
    return 0;
}

size_t
pmgr_group_run(uint8_t sys)
{
    for (ucm_module_t* tmp = UniPMgr->m_list; tmp; tmp = tmp->next) {
        if (tmp->plugin->info.sys == sys) {
            int err = tmp->plugin->run();
            if (err == UCM_RET_SUCCESS) {
                size_t idx       = U__IDX(sys);
                U__REG(sys, idx) = tmp->plugin;
                U__IDX(sys)++;
            } else {
                ucm_etrace("%s - %s: %s\n", _("Broken"), tmp->plugin->info.pid,
                           UniAPI->sys.strerr(err));
            }
        }
    }
    return U__IDX(sys);
}

void
pmgr_group_stop(uint8_t sys)
{
    for (size_t i = 0; U__REG(sys, i) != NULL; i++) {
        U__REG(sys, i)->stop();
        U__REG(sys, i) = NULL;

        U__IDX(sys)--;
    }
}

void
pmgr_unload(void)
{
    UniAPI->sys.rwlock_wlock(UniPMgr->lock);
    while (UniPMgr->m_list) {
        ucm_module_t* tmp = UniPMgr->m_list;
        UniPMgr->m_list   = UniPMgr->m_list->next;

        if (tmp->handle)
            UniAPI->sys.dlclose(tmp->handle);
        ucm_free_null(tmp);
    }
    UniAPI->sys.rwlock_unlock(UniPMgr->lock);

    UniAPI->sys.rwlock_free(UniPMgr->lock);
    ucm_free_null(UniPMgr);
}

const ucm_plugin_t**
pmgr_get(unsigned type)
{
    return (const ucm_plugin_t**)(UniPMgr->registry.items[type]);
}

void
pmgr_message_process(const uint32_t* id, const uintptr_t* ctx, const uint32_t* x1,
                     const uint32_t* x2)
{
    UniAPI->sys.rwlock_rlock(UniPMgr->lock);

    for (size_t i = 0; i < UCM_TYPE_PLUG_STUFF; i++) {
        for (size_t j = 0; U__REG(i, j); j++) {
            if (U__REG(i, j)->message) {
                U__REG(i, j)->message(*id, *ctx, *x1, *x2);
            }
        }
    }

    UniAPI->sys.rwlock_unlock(UniPMgr->lock);
}

#undef U__REG
#undef U__IDX
