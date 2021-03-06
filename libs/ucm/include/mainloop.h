#pragma once

#include "ucm.h"
#include <stdint.h>

int
ucm_mloop_init(int size);
int
ucm_mloop_push(uint32_t id, uintptr_t ctx, uint32_t x1, uint32_t x2);
int
ucm_mloop_pop(uint32_t* id, uintptr_t* ctx, uint32_t* x1, uint32_t* x2);
void
ucm_mloop_clear(void);
void
ucm_mloop_wait(void);
int
ucm_mloop_noempty(void);
void
ucm_mloop_free(void);

ucm_signal_t*
ucm_signal_alloc(uint32_t id);
ucm_signal_t*
ucm_signal_alloc2(uint32_t id, void* ctx, size_t mem);
int
ucm_signal_push(ucm_signal_t* signal, uint32_t x1, uint32_t x2, void* sender);
void
ucm_signal_free(ucm_signal_t** signal);
