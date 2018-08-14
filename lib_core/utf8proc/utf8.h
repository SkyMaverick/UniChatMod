#ifndef _UCM_UTF8_INTERNAL_H_
#define _UCM_UTF8_INTERNAL_H_

#include <stdarg.h>
#include <inttypes.h>
#include "utf8proc.h"

// FIXME This proto
typedef utf8proc_uint8_t  uechar_t;
typedef utf8proc_uint32_t  uchar_t;

// typedef struct {
//     size_t  lenght;     // string lenght with \0 terminated
//     uchar_t data [1];   // unicode hcaracters buffer
// } uchar_t;
// #define RAWSTR(X) (X).data

size_t
ucm_strlen (uchar_t* string);

uchar_t*
ucm_strdup (uchar_t* string);

void
ucm_strcat (uchar_t*  result,
            uchar_t** cat);

int
ucm_strcmp (const uchar_t* lstr,
            const uchar_t* rstr);

int8_t
ucm_strcasecmp (uchar_t* left,
                uchar_t* right);

int8_t
ucm_strncmp (uchar_t* left,
             uchar_t* right,
             size_t        num);

void
ucm_strcpy (uchar_t* dest,
            uchar_t* source);

void
ucm_strncpy (uchar_t* dest,
             uchar_t* source,
             size_t        num);

#endif
