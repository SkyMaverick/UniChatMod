#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "ucm.h"
#include "config.h"
#include "gettext.h"

#include "mdbx.h"

#include "db_mdbx.h"
#include "mdbx_core.h"

static UCM_RET
__mdbx_map (void)
{
    
    return UCM_RET_SUCCESS;
}

UCM_RET
mdbx_db_open  (const char*  file,
               uint32_t     flags)
{
    return UCM_RET_SUCCESS;
}

UCM_RET
mdbx_db_close (void)
{
    // TODO
    return UCM_RET_SUCCESS;
}

UCM_RET
mdbx_db_flush (void)
{
    return UCM_RET_SUCCESS;
}
