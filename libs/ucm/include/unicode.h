#pragma once

#include "ucm.h"
#include "utf8proc.h"

#include <inttypes.h>
#include <stdarg.h>

int64_t
u8_decode_ucs4(ucm_str_t str, const size_t str_len, ucm_wstr_t* ret);

int64_t
ucs4_encode_u8(ucm_wstr_t str, const size_t str_len, ucm_str_t* ret);

size_t
ucm_strlen(ucm_wstr_t str);

ucm_wstr_t
ucm_strdup2(ucm_wstr_t str);

int
ucm_strcmp(ucm_wstr_t lstr, ucm_wstr_t rstr);

int
ucm_strcasecmp(ucm_wstr_t lstr, ucm_wstr_t rstr);

int
ucm_strncmp(ucm_wstr_t lstr, ucm_wstr_t rstr, size_t num);

int
ucm_strncasecmp(ucm_wstr_t lstr, ucm_wstr_t rstr, size_t num);

void
ucm_strupcase(ucm_wstr_t str);

void
ucm_strlowcase(ucm_wstr_t str);

void
ucm_strcpy(ucm_wstr_t dest, ucm_wstr_t src);

void
ucm_strncpy(ucm_wstr_t dest, ucm_wstr_t src, size_t num);

void
ucm_strcat(ucm_wstr_t dest, ucm_wstr_t src);

void
ucm_strncat(ucm_wstr_t dest, ucm_wstr_t src, size_t num);

void
ucm_vstrcat(ucm_wstr_t dest, unsigned num, va_list va);

void
ucm_mstrcat(ucm_wstr_t dest, unsigned num, ...);

ucm_wstr_t
ucm_strchr(ucm_wstr_t str, ucm_wchr_t chr);

ucm_wstr_t
ucm_strrchr(ucm_wstr_t str, ucm_wchr_t chr);

ucm_wstr_t
ucm_strjoin(ucm_wstr_t str1, ucm_wstr_t str2);

ucm_wstr_t
ucm_vstrjoin(size_t num, va_list va);

ucm_wstr_t
ucm_mstrjoin(size_t num, ...);

ucm_wstr_t
ucm_strbrkjoin(ucm_wstr_t str1, ucm_wstr_t str2, ucm_wchr_t brk);

ucm_wstr_t
ucm_vstrbrkjoin(size_t num, ucm_wchr_t brk, va_list va);

ucm_wstr_t
ucm_mstrbrkjoin(ucm_wchr_t brk, size_t num, ...);

int64_t
ucm_strstr(ucm_wstr_t str, ucm_wstr_t sstr);

int64_t
ucm_strcasestr(ucm_wstr_t str, ucm_wstr_t sstr);
