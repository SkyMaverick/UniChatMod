#ifndef _UCM_UNICODE_CONV_H_
#define _UCM_UNICODE_CONV_H_

#include "ucm.h"

u32char*_t u8to_u32  (u8char_t* str);
u8char*_t u32to_u8  (u32char_t* str);

void u8to_u32conv  (u8char_t* str);
void u32to_u8conv  (u32char_t* str);

#endif
