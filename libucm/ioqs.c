#include "ioqs.h"

#include "config.h"
#include "defs.h"
#include "gettext.h"
#include "mqueue.h"
#include "ucm.h"

#include <stdlib.h>
#include <string.h>

static struct mq_block_s* i_msgs;
static struct mq_block_s* o_msgs;
