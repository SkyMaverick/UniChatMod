#ifndef _UCM_UTF8_INTERNAL_H_
#define _UCM_UTF8_INTERNAL_H_

#include <stdarg.h>
#include <inttypes.h>
#include "utf8proc.h"

// FIXME This proto
typedef utf8proc_uint8_t  uechar_t;
typedef utf8proc_uint32_t  uchar_t;

size_t
ucm_strlen (uchar_t* str);

uchar_t*
ucm_strdup (uchar_t* str);

int
ucm_strcmp (uchar_t* lstr,
            uchar_t* rstr);

int
ucm_strcmp (uchar_t* lstr,
            uchar_t* rstr);

int
ucm_strcasecmp (uchar_t* lstr,
                uchar_t* rstr);

int
ucm_strncmp (uchar_t* lstr,
             uchar_t* rstr,
             size_t num);

int
ucm_strncasecmp (uchar_t* lstr,
                 uchar_t* rstr,
                 size_t num);

void
ucm_strupcase (uchar_t* str);

void
ucm_strlowcase (uchar_t* str);

void
ucm_strcpy (uchar_t* dest,
            uchar_t* src);

void
ucm_strncpy (uchar_t* dest,
             uchar_t* src,
             size_t   num);

void
ucm_strcat (uchar_t* dest,
            uchar_t* src);

void
ucm_strncat (uchar_t* dest,
             uchar_t* src,
             size_t   num);

void
ucm_vstrcat (uchar_t* dest,
             unsigned num,
             va_list va);

void
ucm_mstrcat (uchar_t* dest,
             unsigned num,
             ...);

uchar_t*
ucm_strchr (uchar_t* str,
            uchar_t  chr);

uchar_t*
ucm_strrchr (uchar_t* str,
             uchar_t  chr);

uchar_t*
ucm_strjoin (uchar_t* str1,
             uchar_t* str2);

uchar_t*
ucm_vstrjoin (size_t   num,
              va_list  va);

uchar_t*
ucm_mstrjoin (size_t   num,
              ...);

uchar_t*
ucm_strbrkjoin (uchar_t* str1,
                uchar_t* str2,
                uchar_t  brk);

uchar_t*
ucm_vstrbrkjoin (size_t   num,
                 uchar_t  brk,
                 va_list  va);

uchar_t*
ucm_mstrbrkjoin (uchar_t  brk,
                 size_t   num,
                 ...);

int64_t
ucm_strstr (uchar_t* str,
            uchar_t* sstr);

int64_t
ucm_strcasestr (uchar_t* str,
                uchar_t* sstr);
#endif
