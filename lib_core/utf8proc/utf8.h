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

size_t 
ucm_strlen (ucm_string_t* string);

ucm_string_t*
ucm_strdup (ucm_string_t* string);

void
ucm_strcat (ucm_string_t*  result,
            ucm_string_t** cat);

int8_t
ucm_strcmp (ucm_string_t* left,
            ucm_string_t* right);

int8_t
ucm_strcasecmp (ucm_string_t* left,
                ucm_string_t* right);

int8_t
ucm_strncmp (ucm_string_t* left,
             ucm_string_t* right,
             size_t        num);

void
ucm_strcpy (ucm_string_t* dest,
            ucm_string_t* source);

void
ucm_strncpy (ucm_string_t* dest,
             ucm_string_t* source,
             size_t        num);

#endif
