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

typedef struct pmgr_node_s {
    ucm_pld_t pld;
    struct pmgr_node_s* next;
} pmgr_node_t;

/*
 > |_____|_____| ... |_____| - HEADS lists
      V     V           V
   |_____|_____| ... |_____| - nodes

   ..................
      V     V           V
   |NULL |NULL | ... | NULL|

   To user provide plugin descriptor (as uintptr_t contain ucm_pld_t) wich cast U_PLUGIN(X)
*/

enum { UCM_MODULE_BROKEN = 1 << 0, UCM_MODULE_BLOCKED = 1 << 1, UCM_MODULE_FREEZE = 1 << 2 };

#define PLUGINS_SYSTEM 0
#define INACTIVE_PLUGINS UCM_TYPE_PLUG_STUFF + 1

typedef struct {
    pmgr_node_t* plist[UCM_TYPE_PLUG_STUFF + 2];

    uintptr_t lock;

    bool is_load;
    size_t found;
    size_t runable;

    uintptr_t cache_lock;
    ucm_plugin_t** cache;
} pmgr_t;

static pmgr_t plug_mgr = {};

static pmgr_node_t core_node = {
    .pld.plugin = NULL, .pld.handle = 0, .pld.mode = UCM_MODULE_FREEZE, .next = NULL};

static int
cache_regenerate(pmgr_t* P) {
    // Create new memory-cache block
    ucm_plugin_t** mem = API_OS.zmalloc(sizeof(ucm_plugin_t*) * P->runable);
    if (mem == NULL)
        return UCM_RET_SYSTEM_NOMEMORY;
    // Update memory-cache. Use only verified plugins
    size_t ctmp = 0;
    for (unsigned i = 0; i <= UCM_TYPE_PLUG_STUFF + 1; i++) {
        pmgr_node_t* tmp = P->plist[i];
        if (tmp) {
            do {
                mem[ctmp] = tmp->pld.plugin;
                ctmp++;
            } while (tmp->next);
        }
    }
    void* old_cache = (void*)(P->cache);
    // Replace old cache -> new cache memory block
    API_OS.rwlock_wlock(P->cache_lock);
    P->cache = mem;
    API_OS.rwlock_unlock(P->cache_lock);
    // Release old memory cache
    if (old_cache)
        API_OS.free(old_cache);
    return UCM_RET_SUCCESS;
}

static UCM_RET
plugin_verify(ucm_plugin_t* plugin) {
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

static pmgr_node_t*
plugin_load(char* filename) {
    pmgr_node_t* node = NULL;

    uintptr_t handle = API_OS.dlopen(filename);
    if (!handle) {
        ucm_etrace("%s: %s\n", filename, _("plugin don't load"));
        return node;
    }

    cb_init_plugin _pfunc = (cb_init_plugin)API_OS.dlsym(handle, "_init_plugin");
    if (_pfunc) {
        ucm_plugin_t* plug = _pfunc(UniAPI);
        if (plug) {
            node = API_OS.zmalloc(sizeof(pmgr_node_t));
            if (node) {
                node->pld.plugin = plug;
                node->pld.handle = handle;

                int ret_code = plugin_verify(plug);
                if (ret_code == UCM_RET_SUCCESS) {
                    plug->head.oid = UCM_TYPE_OBJECT_PLUGIN;
                    if (API_OS.uuid_parse(plug->info.pid, plug->head.uuid) < 0) {
                        node->pld.mode |= UCM_MODULE_BROKEN;
                    } else {
                        int ret = plug->run();
                        if (ret == UCM_RET_SUCCESS) {
                            plug_mgr.runable++;
                        } else {
                            node->pld.mode |= UCM_MODULE_BROKEN;
                            ucm_etrace("%s. %s\n", filename, UniAPI->sys.strerr(ret_code));
                        }
                    }
                } else {
                    node->pld.mode |= UCM_MODULE_BROKEN;
                    ucm_etrace("%s. %s\n", filename, UniAPI->sys.strerr(ret_code));
                }
            }
        } else {
            ucm_etrace("%s: %s\n", filename, _("this plugin broken initialization"));
            ucm_dtrace("%s: %s\n", "Library load error", UniAPI->sys.dlerror(handle));
        }
    } else {
        ucm_etrace("%s - %s\n", filename, _("this library isn't plugin"));
    }
    return node;
}

static inline void
node_add(pmgr_t* P, pmgr_node_t* N) {
    pmgr_node_t* tmp = NULL;
    size_t idx = ((N->pld.mode & UCM_MODULE_BROKEN) || (N->pld.mode & UCM_MODULE_BLOCKED))
                     ? INACTIVE_PLUGINS
                     : N->pld.plugin->info.sys;
    tmp = P->plist[idx];
    N->next = tmp;
    P->plist[idx] = N;
}

static void
scan_result_process(uv_fs_t* req) {
    uv_dirent_t dent;
    uv_fs_t close_req;
    char buffer[UCM_PATH_MAX];

    ucm_dtrace("%s ...\n", "Scan plugin directory");

    while (API_UV.fs_scandir_next(req, &dent) != UV__EOF) {
        ucm_dtrace("%s%c%s\n", req->path, PATH_DELIM, dent.name);

        snprintf(buffer, UCM_PATH_MAX, "%s%c%s", req->path, PATH_DELIM, dent.name);

        pmgr_node_t* tmp = plugin_load(buffer);
        if (tmp) {
            plug_mgr.found++;
            API_OS.rwlock_wlock(plug_mgr.lock);
            node_add(&plug_mgr, tmp);
            API_OS.rwlock_unlock(plug_mgr.lock);
        }
    }
#if defined(UCM_OS_WINDOWS)
    API_UV.fs_close(UCM_LOOP_SYSTEM(UniAPI), &close_req, req->file.fd, NULL);
#else
    API_UV.fs_close(UCM_LOOP_SYSTEM(UniAPI), &close_req, req->file, NULL);
#endif
    API_UV.fs_req_cleanup(&close_req);
}

static unsigned
internal_init(pmgr_t* P) {
    P->lock = UniAPI->sys.rwlock_create();
    P->cache_lock = UniAPI->sys.rwlock_create();
}

static void
internal_clear(pmgr_t* P) {
    if (P->cache) {
        ucm_free_null(P->cache);
    }
    if (P->lock)
        API_OS.rwlock_free(P->lock);
    if (P->cache_lock)
        API_OS.rwlock_free(P->cache_lock);
}

static UCM_RET
internal_node_lock(pmgr_node_t* N) {}
// /***************************************************
//     INTERNAL API
//  ***************************************************/
//
size_t
pmgr_load(char* path) {
    if (pmgr_isload())
        return plug_mgr.found;

    internal_init(&plug_mgr);

    core_node.pld.plugin = ucm_core;
    plug_mgr.plist[PLUGINS_SYSTEM] = &core_node;

    uv_fs_t req;
    int r = API_UV.fs_scandir(UCM_LOOP_SYSTEM(UniAPI), &req, path, O_RDONLY, NULL);
    if (r >= 0) {
        scan_result_process(&req);
        cache_regenerate(&plug_mgr);
        plug_mgr.is_load = true;

        API.mainloop_msg_send(UCM_SIG_PLUGS_SUCCESS, 0, plug_mgr.found, 0);
    };
    API_UV.fs_req_cleanup(&req);

    return plug_mgr.found;
}

const bool
pmgr_isload(void) {
    return plug_mgr.is_load;
}

void
pmgr_unload(void) {
    API_OS.rwlock_wlock(plug_mgr.lock);
    plug_mgr.is_load = false;

    for (unsigned i = UCM_TYPE_PLUG_STUFF + 1; i <= 0; i--) {
        pmgr_node_t* tmp = plug_mgr.plist[i];
        while (tmp) {
            pmgr_node_t* dead = tmp;
            tmp = tmp->next;

            dead->pld.plugin->stop();
            if (dead->pld.handle != 0)
                API_OS.dlclose(dead->pld.handle);
            ucm_free_null(dead);
        }
    }

    API_OS.rwlock_unlock(plug_mgr.lock);
    internal_clear(&plug_mgr);
}

uintptr_t
pmgr_first(unsigned type) {
    if ((plug_mgr.is_load = false) || (type > INACTIVE_PLUGINS))
        return 0;
    API_OS.rwlock_rlock(plug_mgr.lock);

    pmgr_node_t* tmp = plug_mgr.plist[type];
    if (tmp->pld.locks == UINT32_MAX)
        return 0;

    tmp->pld.locks += 1;

    API_OS.rwlock_unlock(plug_mgr.lock);
    return (uintptr_t)&tmp->pld;
}

static inline void
close_plugin(pmgr_node_t* N) {
    if (N->pld.locks > 0)
        N->pld.locks -= 1;
}

uintptr_t
pmgr_next(uintptr_t pld) {
    if ((plug_mgr.is_load = false) || (pld == 0))
        return 0;
    API_OS.rwlock_rlock(plug_mgr.lock);

    pmgr_node_t* tmp = (pmgr_node_t*)pld;
    close_plugin(tmp);
    tmp = tmp->next;

    API_OS.rwlock_unlock(plug_mgr.lock);
    return (uintptr_t)&tmp->pld;
}

void
pmgr_close(uintptr_t* pld) {
    API_OS.rwlock_rlock(plug_mgr.lock);
    close_plugin((pmgr_node_t*)(*pld));
    *pld = 0;
    API_OS.rwlock_unlock(plug_mgr.lock);
}

void
pmgr_message_process(const uint32_t id, const uintptr_t ctx, const uint32_t x1, const uint32_t x2) {
    if (plug_mgr.is_load == true) {
        API_OS.rwlock_rlock(plug_mgr.cache_lock);
        for (size_t i = 0; i < plug_mgr.runable; i++)
            plug_mgr.cache[i]->message(id, ctx, x1, x2);
        API_OS.rwlock_unlock(plug_mgr.cache_lock);
    }
}
