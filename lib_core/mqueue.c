#include "ucm.h"
#include "api.h"
#include "mqueue.h"
#include "threading.h"

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
    uintptr_t mutex;
    uintptr_t cond;
    mq_msg_t queue[1];
} mq_block_t;

static void
_mq_flush (mq_block_t* h)
{
    h->head = 0; h->tail = 0; h->count = 0;
    ucm_zmemory (h->queue, h->q_size * sizeof(mq_msg_t));
};

struct mq_block_s*
mq_create (uint32_t q_count)
{
    int mem_sz = sizeof(mq_block_t) + (q_count - 1) * sizeof(mq_msg_t);
    mq_block_t* h = ucm_zmalloc (mem_sz);
    h->q_size = q_count;
    h->mutex = mutex_create ();
    h->cond = cond_create ();
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
    mutex_lock (h->mutex);
    if (h->count < h->q_size){

        h->queue[h->tail].id  = id;
        h->queue[h->tail].ctx = ctx;
        h->queue[h->tail].x1  = x1;
        h->queue[h->tail].x2  = x2;

        h->tail++; h->count++;
        if (h->tail == h->q_size) h->tail = 0;
    } else {
        mutex_unlock (h->mutex);
        return UCM_RET_OVERFLOW;
    }
    cond_signal (h->cond);
    mutex_unlock (h->mutex);
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
        mutex_lock (h->mutex);
            if (h->count > 0){

                *id =  h->queue[h->head].id;
                *ctx = h->queue[h->head].ctx;
                *x1 =  h->queue[h->head].x1;
                *x2 =  h->queue[h->head].x2;

                h->count--; h->head++;
                if (h->head == h->q_size) h->head = 0;
            } else {
                mutex_unlock (h->mutex);
                return UCM_RET_EMPTY;
            }
        mutex_unlock (h->mutex);
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
    cond_wait(h->cond, h->mutex);
    mutex_unlock(h->mutex);
};

int
mq_noempty (mq_block_t *h)
{
    return h->count > 0 ? 1:0;
};

void
mq_free (mq_block_t *h)
{
    mutex_lock (h->mutex);
    _mq_flush (h);
    mutex_unlock (h->mutex);
    mutex_free (h->mutex);
    cond_free (h->cond);
    ucm_free (h);
};
