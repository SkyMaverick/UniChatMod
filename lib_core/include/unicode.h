#ifndef _UCM_UNICODE_FUNCS_H_
#define _UCM_UNICODE_FUNCS_H_

#include <stdarg.h>
#include <inttypes.h>
#include "utf8proc.h"
#include "ucm.h"


int64_t
u8_decode_ucs4 (u8char_t*       str,
                const int64_t   str_len,
                u32char_t**     ret);

int64_t
ucs4_encode_u8 (u32char_t*       str,
                const int64_t    str_len,
                u8char_t**       ret);

size_t
ucm_strlen (u32char_t* str);

u32char_t*
ucm_strdup2 (u32char_t* str);

int
ucm_strcmp (u32char_t* lstr,
            u32char_t* rstr);

int
ucm_strcasecmp (u32char_t* lstr,
                u32char_t* rstr);

int
ucm_strncmp (u32char_t* lstr,
             u32char_t* rstr,
             size_t num);

int
ucm_strncasecmp (u32char_t* lstr,
                 u32char_t* rstr,
                 size_t num);

void
ucm_strupcase (u32char_t* str);

void
ucm_strlowcase (u32char_t* str);

void
ucm_strcpy (u32char_t* dest,
            u32char_t* src);

void
ucm_strncpy (u32char_t* dest,
             u32char_t* src,
             size_t   num);

void
ucm_strcat (u32char_t* dest,
            u32char_t* src);

void
ucm_strncat (u32char_t* dest,
             u32char_t* src,
             size_t   num);

void
ucm_vstrcat (u32char_t* dest,
             unsigned num,
             va_list va);

void
ucm_mstrcat (u32char_t* dest,
             unsigned num,
             ...);

u32char_t*
ucm_strchr (u32char_t* str,
            u32char_t  chr);

u32char_t*
ucm_strrchr (u32char_t* str,
             u32char_t  chr);

u32char_t*
ucm_strjoin (u32char_t* str1,
             u32char_t* str2);

u32char_t*
ucm_vstrjoin (size_t   num,
              va_list  va);

u32char_t*
ucm_mstrjoin (size_t   num,
              ...);

u32char_t*
ucm_strbrkjoin (u32char_t* str1,
                u32char_t* str2,
                u32char_t  brk);

u32char_t*
ucm_vstrbrkjoin (size_t   num,
                 u32char_t  brk,
                 va_list  va);

u32char_t*
ucm_mstrbrkjoin (u32char_t  brk,
                 size_t   num,
                 ...);

int64_t
ucm_strstr (u32char_t* str,
            u32char_t* sstr);

int64_t
ucm_strcasestr (u32char_t* str,
                u32char_t* sstr);
#endif
