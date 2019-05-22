#include <stdlib.h>
#include <string.h>

#include "ucm.h"
#include "api.h"

#include "defs.h"
#include "config.h"
#include "mqueue.h"
#include "mainloop.h"
#include "gettext.h"

static struct mq_block_s* messages;

int
ucm_mloop_init (int size)
{
    if (size < UCM_DEF_MQ_LIMIT) {
        ucm_etrace("%s %d. %s: %d. %s\n",
                  _("Message queue size don't less minimal size"),
                  UCM_DEF_MQ_LIMIT,
                  _("Now define size"),
                  size,
                  _("Using default value"));
        size = UCM_DEF_MQ_LIMIT;
    }
    messages = mq_create(size);
    return messages ? UCM_RET_SUCCESS :
                      UCM_RET_NOOBJECT;
}

int
ucm_mloop_push (uint32_t  id,
                uintptr_t ctx,
                uint32_t  x1,
                uint32_t  x2)
{
    return mq_push (messages, id, ctx, x1, x2);
}

int
ucm_mloop_pop (uint32_t*  id,
               uintptr_t* ctx,
               uint32_t*  x1,
               uint32_t*  x2)
{
    return mq_pop (messages, id, ctx, x1, x2);
}

void
ucm_mloop_clear (void)
{
    mq_clear (messages);
}

void
ucm_mloop_wait (void)
{
    mq_wait(messages);
}

int
ucm_mloop_noempty (void)
{
    return mq_noempty (messages);
}

void
ucm_mloop_free (void)
{
    if (messages)
        mq_free (messages);
}

static inline size_t
event_size_get (int id) {

    switch (id) {
        case UCM_EVENT_START_GUI:
        case UCM_EVENT_START_GUI2:
            return sizeof (ucm_evgui_t);
        default:
            return 0;
    }
}

ucm_ev_t*
ucm_mloop_event_alloc2 (int     id,
                        void*   ctx,
                        size_t  mem)
{
    ucm_ev_t* event = NULL;

    size_t size = event_size_get(id);
    if (size) {
        ucm_dtrace("%s: %d. %s: %d\n","Event alloc",id,"Allocated",size);
        event = UniAPI->sys.zmalloc (size + mem);
        if (event) {
            // Define EVENT arguments
            event->oid      = UCM_TYPE_OBJECT_EVENT;
            event->ev       = id;
            event->size     = size + mem;
            event->sender   = NULL;
            event->ctx      = event + size;

            // copy context
            if (ctx)
                memcpy (event->ctx, ctx, mem);
        }
    }
    return NULL;
}

ucm_ev_t*
ucm_mloop_event_alloc (int id)
{
    return ucm_mloop_event_alloc2 (id, NULL,  0);
}

void
ucm_mloop_event_free (ucm_ev_t** event)
{
    if (*event) {
        ucm_dtrace("Free event: %d\n",(*event)->ev);
        switch ((*event)->ev){
            // TODO
        }
        ucm_free_null(*event);
    }
}

int
ucm_mloop_event_push (ucm_ev_t* event,
                      uint32_t x1,
                      uint32_t x2,
                      void*    sender)
{
    if (!event)
        return UCM_RET_NOOBJECT;

    event->sender = sender;
    return mq_push (messages, event->ev, (uintptr_t)event, x1, x2);
}
