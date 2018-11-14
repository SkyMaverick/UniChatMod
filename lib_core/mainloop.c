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

ucm_ev_t*
ucm_mloop_event_alloc (int id)
{
    size_t size = 0;
    ucm_ev_t* event = NULL;
    switch (id) {
        case UCM_EVENT_START_GUI:
        case UCM_EVENT_START_GUI2:
            {
                size = sizeof (ucm_evgui_t);
                break;
            };
        default:
            {
                ucm_etrace("%s: %d. %s\n", _("Don't create event:"),id,_("Use message push interface"));
                return NULL;
            };
    }

    if (size) {
        ucm_dtrace("%s: %d. %s: %d\n","Event alloc",id,"Allocated",size);
        event = ucm_kzmalloc(size);

        event->oid = UCM_TYPE_OBJECT_EVENT;
        event->ev = id;
        event->size = size;
        event->sender = NULL;
    };

    return event;
}

void
ucm_mloop_event_free (ucm_ev_t** event)
{
    if (*event) {
        ucm_dtrace("Free event: %d\n",(*event)->ev);
        switch ((*event)->ev){
            // TODO
        }
        ucm_kfree_null(*event);
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
