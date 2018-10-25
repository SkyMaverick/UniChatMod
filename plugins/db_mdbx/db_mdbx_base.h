#ifndef _DB_MDBX_BASE_OPERATIONS_H_
#define _DB_MDBX_BASE_OPERATIONS_H_

#include "ucm.h"
#include "libmdbx/mdbx.h"

UCM_RET
db_open (db_object_t* db);

UCM_RET
db_flush (db_object_t* db);

UCM_RET
db_check (db_object_t* db);

static inline int
settings_cmp (const MDBX_val* left,
              const MDBX_val* right)
{
    // TODO
    UNUSED (left);
    UNUSED (right);
    return 0;
}

#endif
