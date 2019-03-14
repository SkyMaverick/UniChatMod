#include "ucm.h"

UCM_RET
mdbx_db_open  (const char*  file,
               uint32_t     flags);
UCM_RET
mdbx_db_close (void);
void
mdbx_db_flush (bool force);
