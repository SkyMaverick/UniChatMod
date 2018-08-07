#ifndef _UCM_UTF8_INTERNAL_H_
#define _UCM_UTF8_INTERNAL_H_

#include <vargs.h>
#include "utf8proc.h"

int
u_printf (const char* format, 
          ...);

int
u_vprintf (const char* format,
           va_list     ap);

#endif
