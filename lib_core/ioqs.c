#include <stdlib.h>
#include <string.h>

#include "ucm.h"
#include "defs.h"
#include "config.h"
#include "mqueue.h"
#include "ioqs.h"
#include "gettext.h"

static struct mq_block_s* i_msgs;
static struct mq_block_s* o_msgs;
