#pragma once

#include "ucm.h"

// enum {
//     PMGR_FLAG_NORMAL     = 0,
// //    PMGR_FLAG_ONLYINIT   = 1 << 0,
// };
// 
size_t
pmgr_load (char*    path,
           uint32_t flags);
void
pmgr_unload (void);
const ucm_plugin_t**
pmgr_get (unsigned type);
void
pmgr_message_process (const uint32_t* id,
                      const uintptr_t* ctx,
                      const uint32_t* x1,
                      const uint32_t* x2);
size_t
pmgr_group_run (uint8_t sys);
void
pmgr_group_stop (uint8_t sys);
