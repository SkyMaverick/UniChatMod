#pragma once

#include "ucm.h"

UCM_RET
plugins_load_registry (const char* plug_path);

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
const ucm_plugin_t**
plugins_get_all (void);

const ucm_plugin_t**
plugins_get_db (void);

const ucm_plugin_t**
plugins_get_proto (void);

const ucm_plugin_t**
plugins_get_crypt (void);

const ucm_plugin_t**
plugins_get_hist (void);

const ucm_plugin_t**
plugins_get_gui (void);

const ucm_plugin_t**
plugins_get_stuff (void);

const size_t
plugins_count (void);
