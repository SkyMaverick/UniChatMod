#ifndef _UCM_UTF8_INTERNAL_H_
#define _UCM_UTF8_INTERNAL_H_

#include <stdarg.h>
#include <inttypes.h>
#include "utf8proc.h"
#include "uconv.h"

// FIXME This proto
typedef utf8proc_uint8_t   u8char_t;
typedef utf8proc_uint16_t  u16char_t;
typedef utf8proc_uint32_t  u32char_t;

size_t
u32_strlen (u32char_t* str);

u32char_t*
u32_strdup (u32char_t* str);

int
u32_strcmp (u32char_t* lstr,
            u32char_t* rstr);

int
u32_strcmp (u32char_t* lstr,
            u32char_t* rstr);

int
u32_strcasecmp (u32char_t* lstr,
                u32char_t* rstr);

int
u32_strncmp (u32char_t* lstr,
             u32char_t* rstr,
             size_t num);

int
u32_strncasecmp (u32char_t* lstr,
                 u32char_t* rstr,
                 size_t num);

void
u32_strupcase (u32char_t* str);

void
u32_strlowcase (u32char_t* str);

void
u32_strcpy (u32char_t* dest,
            u32char_t* src);

void
u32_strncpy (u32char_t* dest,
             u32char_t* src,
             size_t   num);

void
u32_strcat (u32char_t* dest,
            u32char_t* src);

void
u32_strncat (u32char_t* dest,
             u32char_t* src,
             size_t   num);

void
u32_vstrcat (u32char_t* dest,
             unsigned num,
             va_list va);

void
u32_mstrcat (u32char_t* dest,
             unsigned num,
             ...);

u32char_t*
u32_strchr (u32char_t* str,
            u32char_t  chr);

u32char_t*
u32_strrchr (u32char_t* str,
             u32char_t  chr);

u32char_t*
u32_strjoin (u32char_t* str1,
             u32char_t* str2);

u32char_t*
u32_vstrjoin (size_t   num,
              va_list  va);

u32char_t*
u32_mstrjoin (size_t   num,
              ...);

u32char_t*
u32_strbrkjoin (u32char_t* str1,
                u32char_t* str2,
                u32char_t  brk);

u32char_t*
u32_vstrbrkjoin (size_t   num,
                 u32char_t  brk,
                 va_list  va);

u32char_t*
u32_mstrbrkjoin (u32char_t  brk,
                 size_t   num,
                 ...);

int64_t
u32_strstr (u32char_t* str,
            u32char_t* sstr);

int64_t
u32_strcasestr (u32char_t* str,
                u32char_t* sstr);
#endif
