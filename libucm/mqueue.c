#include "ucm.h"
#include "api.h"
#include "defs.h"
#include "mqueue.h"

#include <stdlib.h>
#include <string.h>

typedef struct mq_msg_s {
    uint32_t id;
    uintptr_t ctx;
    uint32_t x1;
    uint32_t x2;
} mq_msg_t;

typedef struct mq_block_s {
    uint32_t q_size;
    uint32_t count;
    uint32_t head;
    uint32_t tail;

    uintptr_t cond;
    uintptr_t mtx;

    mq_msg_t queue[1];
} mq_block_t;

static void
_mq_flush (mq_block_t* h)
{
    h->head = 0; h->tail = 0; h->count = 0;
    UniAPI->sys.zmemory (h->queue, h->q_size * sizeof(mq_msg_t));
};

struct mq_block_s*
mq_create (uint32_t q_count)
{
    int mem_sz = sizeof(mq_block_t) + (q_count - 1) * sizeof(mq_msg_t);
    mq_block_t* h = UniAPI->sys.zmalloc (mem_sz);
    h->q_size = q_count;

    h->mtx  = UniAPI->sys.mutex_create ();
    h->cond = UniAPI->sys.cond_create  ();
    
    _mq_flush(h);
    return h;
};

int
mq_push (mq_block_t* h,
         uint32_t   id,
         uintptr_t  ctx,
         uint32_t   x1,
         uint32_t   x2)
{
if(h){
    UniAPI->sys.mutex_lock (h->mtx);
    if (h->count < h->q_size){

        h->queue[h->tail].id  = id;
        h->queue[h->tail].ctx = ctx;
        h->queue[h->tail].x1  = x1;
        h->queue[h->tail].x2  = x2;

        h->tail++; h->count++;
        if (h->tail == h->q_size) h->tail = 0;
    } else {
        UniAPI->sys.mutex_unlock (h->mtx);
        return UCM_RET_OVERFLOW;
    }
    UniAPI->sys.cond_signal (h->cond);
    UniAPI->sys.mutex_unlock (h->mtx);
    return UCM_RET_SUCCESS;
}else
    return UCM_RET_NOOBJECT;
};

int
mq_pop (mq_block_t* h,
        uint32_t*  id,
        uintptr_t* ctx,
        uint32_t*  x1,
        uint32_t*  x2)
{
    if (h){
        UniAPI->sys.mutex_lock (h->mtx);
            if (h->count > 0){

                *id =  h->queue[h->head].id;
                *ctx = h->queue[h->head].ctx;
                *x1 =  h->queue[h->head].x1;
                *x2 =  h->queue[h->head].x2;

                h->count--; h->head++;
                if (h->head == h->q_size) h->head = 0;
            } else {
                UniAPI->sys.mutex_unlock (h->mtx);
                return UCM_RET_EMPTY;
            }
        UniAPI->sys.mutex_unlock (h->mtx);
        return UCM_RET_SUCCESS;
    }else
        return UCM_RET_NOOBJECT;
};

void
mq_clear (mq_block_t *h)
{
    if (!h)
        return;
    _mq_flush (h);
};

void
mq_wait (mq_block_t *h)
{
    UniAPI->sys.mutex_lock(h->mtx);
    UniAPI->sys.cond_wait(h->cond, h->mtx);
    UniAPI->sys.mutex_unlock(h->mtx);
};

int
mq_noempty (mq_block_t *h)
{
    return h->count > 0 ? 1:0;
};

void
mq_free (mq_block_t *h)
{
    UniAPI->sys.mutex_lock (h->mtx);
    _mq_flush (h);
    UniAPI->sys.mutex_unlock (h->mtx);
    UniAPI->sys.cond_free (h->cond);
    UniAPI->sys.mutex_free (h->mtx);
    UniAPI->sys.free (h);
};
