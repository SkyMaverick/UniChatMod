#ifndef _MQUEUE_H_
#define _MQUEUE_H_
#include "stdint.h"

/*! create new messages queue and allocate memory for this */
struct mq_block_s* mq_create (uint32_t q_count);

/*! push message in messsages queue bottom */
int mq_push (struct mq_block_s *h, uint32_t id,uintptr_t ctx, uint32_t x1, uint32_t x2);

/*! pop message in messages queue top */
int mq_pop (struct mq_block_s *h,uint32_t* id,uintptr_t* ctx, uint32_t* x1, uint32_t* x2);

/*! clear and remake messages queue */
void mq_clear (struct mq_block_s *h);

/*! wait all threads yet not push messages */
void mq_wait (struct mq_block_s *h);

/*! validate queue emptyness */
int mq_noempty (struct mq_block_s *h);

/*! release messages queue */
void mq_free (struct mq_block_s *h);

#endif
