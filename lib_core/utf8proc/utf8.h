#ifndef _UCM_UTF8_INTERNAL_H_
#define _UCM_UTF8_INTERNAL_H_

#include <stdarg.h>
#include <inttypes.h>
#include "utf8proc.h"

// FIXME This proto
typedef utf8proc_uint8_t  uechar_t;
typedef utf8proc_int32_t  uchar_t;

typedef struct {
    size_t  lenght;     // string lenght with \0 terminated
    uchar_t data [1];   // unicode hcaracters buffer
} ucm_string_t;
#define RAWSTR(X) (X).data



// /*
//  TODO
//  strlen,
//  strcmp,
//  strchr,
//  strcpy,
//  strdup,
//  str2buf,
//  <S-F12>
//  */

#endif
