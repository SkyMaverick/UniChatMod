#ifndef _UCM_PLUGMGR_H_
#define _UCM_PLUGMGR_H_

#include "ucm.h"

UCM_RET
plugins_load_registry (char* plug_path);

void
plugins_release_registry (void);

#endif
