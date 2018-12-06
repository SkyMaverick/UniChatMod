#ifndef MAINLOOP_H_
#define MAINLOOP_H_
#include <stdint.h>

int  ucm_mloop_init (int size);
int  ucm_mloop_push (uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2);
int  ucm_mloop_pop  (uint32_t* id, uintptr_t* ctx, uint32_t* x1, uint32_t* x2);
void ucm_mloop_clear (void);
void ucm_mloop_wait (void);
int  ucm_mloop_noempty (void);
void ucm_mloop_free (void);

ucm_ev_t* ucm_mloop_event_alloc (int id);
int       ucm_mloop_event_push (ucm_ev_t* event, uint32_t x1, uint32_t x2, void* sender);
void      ucm_mloop_event_free (ucm_ev_t** event);
#endif
