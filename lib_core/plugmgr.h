#ifndef _UCM_PLUGMGR_H_
#define _UCM_PLUGMGR_H_

#include "ucm.h"

UCM_RET
plugins_load_registry (char* plug_path);

void
plugins_release_registry (void);

void
plugins_run_all (void);

void
plugins_stop_all (void);

void
plugins_message_dispatch (const uint32_t* id,
                          const uintptr_t* ctx,
                          const uint32_t* x1,
                          const uint32_t* x2);
const ucm_plugin_t*
plugins_get_all (void);

const ucm_plugin_t*
plugins_get_db (void);

const ucm_plugin_t*
plugins_get_net (void);

const ucm_plugin_t*
plugins_get_crypt (void);

const ucm_plugin_t*
plugins_get_hist (void);

const ucm_plugin_t*
plugins_get_stuff (void);

#endif
