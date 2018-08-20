#include "unicode.h"
#include "alloc.h"

#include <stdarg.h>
#include <string.h>

#define CHAR_SIZE sizeof(u32char_t)

#define MIN2(A,B) ((A) < (B) ? (A) : (B))

#define MIN3(A,B,C)(((A) < (B)) ? \
                    ((A) < (C)) ? (A) : (C) : \
                    ((B) < (C)) ? (B) : (C) )

// ==========================================
//     INTERNAL INLINE FUNCTIONS
// ==========================================

// WARNING! without NULL-terminated symbol
static inline size_t
_intrnl_u32strlen (u32char_t* str)
{
    size_t count = 0;
    if (str) {
        for (; *str; str++, count++){};
    }
    return count;
}

static inline void
_intrnl_u32strcpy (u32char_t* dest,
                   u32char_t* src,
                   u32char_t  echr)
{
    size_t i;
    if (dest && src) {
        for (i = 0; src[i] != 0; i++)
            dest [i] = src [i];
        dest [i] = echr;
    }
}

static inline void
_intrnl_u32strncpy (u32char_t* dest,
                    u32char_t* src,
                    u32char_t  echr,
                    size_t   num)
{
    size_t i;
    if (dest && src) {
        for (i = 0; i < num && src[i] != 0; i++)
            dest [i] = src [i];
        dest [i] = echr;
    }
}

// ==========================================
//     BASIC FUNCTIONS
// ==========================================

size_t
u32_strlen (u32char_t* str)
{
    return _intrnl_u32strlen (str);
}

u32char_t*
u32_strdup (u32char_t* str)
{
    u32char_t* buf = NULL;

    if (str) {
        size_t buf_size = (u32_strlen (str) + 1)  * CHAR_SIZE;
        buf = ucm_malloc (buf_size);
        if (buf)
            memcpy (buf, str, buf_size);
    }
    return buf;
}

// ==========================================
//     COMPARE FUNCTIONS
// ==========================================

int
u32_strcmp (u32char_t* lstr,
            u32char_t* rstr)
{
    while (*lstr && (*lstr == *rstr)) lstr++, rstr++;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
u32_strcasecmp (u32char_t* lstr,
                u32char_t* rstr)
{
    while (*lstr && (utf8proc_tolower(*lstr) == utf8proc_tolower(*rstr)))
            lstr++, rstr++;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
u32_strncmp (u32char_t* lstr,
             u32char_t* rstr,
             size_t num)
{
    if (num == 0) return 0;
    while (*lstr && (*lstr == *rstr) && num) lstr++, rstr++, num--;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
u32_strncasecmp (u32char_t* lstr,
                 u32char_t* rstr,
                 size_t num)
{
    if (num == 0) return 0;
    while (*lstr && (utf8proc_tolower(*lstr) == utf8proc_tolower(*rstr)) && num)
            lstr++, rstr++, num--;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

// ==========================================
//     CASEFOLD FUNCTIONS
// ==========================================

void
u32_strupcase (u32char_t* str)
{
    while(*str)
        utf8proc_toupper(*str++);
}

void
u32_strlowcase (u32char_t* str)
{
    while(*str)
        utf8proc_tolower(*str++);
}

// ==========================================
//     STRING COPY FUNCTIONS
// ==========================================

void
u32_strcpy (u32char_t* dest,
            u32char_t* src)
{
    _intrnl_u32strcpy (dest, src, 0);
}

void
u32_strncpy (u32char_t* dest,
             u32char_t* src,
             size_t   num)
{
    _intrnl_u32strncpy (dest, src, 0, num);
}

// ==========================================
//     STRING CONCAT FUNCTIONS
// ==========================================

void
u32_strcat (u32char_t* dest,
            u32char_t* src)
{
    size_t dest_size = _intrnl_u32strlen(dest);
    _intrnl_u32strcpy (dest + dest_size, src, 0);
}

void
u32_strncat (u32char_t* dest,
             u32char_t* src,
             size_t   num)
{
    size_t dest_size = _intrnl_u32strlen(dest);
    _intrnl_u32strncpy (dest + dest_size, src, 0, num);
}

void
u32_vstrcat (u32char_t* dest,
             unsigned num,
             va_list va)
{
    size_t i = _intrnl_u32strlen(dest) + 1;
    size_t j;

    while (num--) {
        u32char_t* tmp_str = va_arg (va, u32char_t*);
        if (tmp_str) {
            for (j = 0; tmp_str[j] != 0; i++, j++)
                dest [i] = tmp_str[j];
            dest [i] = 0;
        }
    }
}

void
u32_mstrcat (u32char_t* dest,
             unsigned num,
             ...)
{
    va_list strs;
    va_start (strs, num);
    u32_vstrcat (dest, num, strs);
    va_end (strs);
}

// ==========================================
//     STRING GET CHAR POSITION FUNCTIONS
// ==========================================

u32char_t*
u32_strchr (u32char_t* str,
            u32char_t  chr)
{
    if (!str)
        return NULL;

    size_t i;
    for (i = 0; str[i] != chr; i++){};
    return &str[i];
}

u32char_t*
u32_strrchr (u32char_t* str,
             u32char_t  chr)
{
    if (!str)
        return NULL;

    size_t i, size = _intrnl_u32strlen (str);

    for (i = size; str [i] != chr; i++){};
    return &str[i];
}

// ==========================================
//     STRING JOIN FUNCTIONS
// ==========================================

u32char_t*
u32_strjoin (u32char_t* str1,
             u32char_t* str2)
{
    u32char_t* result = NULL;
    if (str1 && str2) {
        size_t str1_len = _intrnl_u32strlen (str1);
        size_t str2_len  = _intrnl_u32strlen (str2);

        result = ucm_zmalloc ((str1_len + str2_len + 1) * CHAR_SIZE);

        if ( result ) {
            _intrnl_u32strcpy (result, str1, 0);
            _intrnl_u32strcpy (result + str1_len, str2, 0);
        }
    }
    return result;
}

u32char_t*
u32_vstrjoin (size_t   num,
              va_list  va)
{
    u32char_t* buf = NULL;
    size_t   buf_len  = 0;
    va_list  tmp_va;

    // calculate buffer size
    va_copy (tmp_va, va);
    for (size_t i = 0; i < num; i++) {
        u32char_t* tmp_buf = va_arg (tmp_va, u32char_t*);
        if (tmp_buf && *tmp_buf)
            buf_len += _intrnl_u32strlen(tmp_buf);
    }
    va_end (tmp_va);

    // copy to buffer
    buf = ucm_zmalloc ((buf_len + 1) * CHAR_SIZE);
    if ( buf ) {
        u32char_t* p = buf;
        for (size_t i = 0; i < num; i++) {
            u32char_t* tmp_buf = va_arg (va, u32char_t*);
            if (tmp_buf && *tmp_buf) {
                _intrnl_u32strcpy (p, tmp_buf, 0);
                p += _intrnl_u32strlen(tmp_buf);
            }
        }
    }
    return buf;
}

u32char_t*
u32_mstrjoin (size_t   num,
              ...)
{
    u32char_t* ret = NULL;

    va_list strs;
    va_start (strs, num);

    ret = u32_vstrjoin (num, strs);

    va_end (strs);
    return ret;
}

// ==========================================
//     STRING JOIN WITH BROKEN CHAR FUNCTIONS
// ==========================================

u32char_t*
u32_strbrkjoin (u32char_t* str1,
                u32char_t* str2,
                u32char_t  brk)
{
    u32char_t* result = NULL;
    if (str1 && str2) {
        size_t str1_len = _intrnl_u32strlen (str1);
        size_t str2_len = _intrnl_u32strlen (str2);

        result = ucm_zmalloc( (str1_len + str2_len + 1) * CHAR_SIZE);
        if ( result ) {
            _intrnl_u32strcpy (result, str1, brk);
            _intrnl_u32strcpy (result + str1_len + 1, str2, 0);
        }
    }
    return result;
}

u32char_t*
u32_vstrbrkjoin (size_t   num,
                 u32char_t  brk,
                 va_list  va)
{
    u32char_t* buf = NULL;
    size_t   buf_len  = 0;
    va_list  tmp_va;

    // calculate buffer size
    va_copy (tmp_va, va);
    for (size_t i = 0; i < num; i++) {
        u32char_t* tmp_buf = va_arg (tmp_va, u32char_t*);
        if (tmp_buf && *tmp_buf)
            buf_len += _intrnl_u32strlen(tmp_buf)+1;
    }
    va_end (tmp_va);

    // copy to buffer
    buf = ucm_zmalloc (buf_len * CHAR_SIZE);
    if ( buf ) {
        u32char_t* p = buf;
        for (size_t i = 0; i < num; i++) {
            u32char_t* tmp_buf = va_arg (va, u32char_t*);
            if (tmp_buf && *tmp_buf) {
                _intrnl_u32strcpy (p, tmp_buf, brk);
                p += _intrnl_u32strlen(tmp_buf) + 1;
            }
        }
    }
    return buf;
}

u32char_t*
u32_mstrbrkjoin (u32char_t  brk,
                 size_t   num,
                 ...)
{
    u32char_t* ret = NULL;

    va_list strs;
    va_start (strs, num);

    ret = u32_vstrbrkjoin (num, brk, strs);

    va_end (strs);
    return ret;

}

// ==========================================
//      SUBSTRING SEARCH FUNCTIONS
// ==========================================

int64_t
u32_strstr (u32char_t* str,
            u32char_t* sstr)
{
    if (str && sstr) {
        int64_t i, j, N, M;
        N = _intrnl_u32strlen(str);
        M = _intrnl_u32strlen(sstr);

        int64_t* d = ucm_zmalloc (M * sizeof(int64_t));

        // prefix function
        for (i = 1, j = 0; i < M; i++)
        {
            while ( j > 0 && sstr[j] != sstr[i])
                j = d [j-1];
            if (sstr [j] == sstr [i])
                j++;
            d [i] = j;
        }

        // Search function
        for (i = 0, j = 0; i < N; i++)
        {
            while ( j > 0 && sstr [j] != str [i] )
                j = d [j - 1];
            if ( sstr [j] == str [i] )
                j++;
            if ( j == M) {
                ucm_free(d);
                return i - j + 1;
            }
        }
        ucm_free (d);
    }
    return -1;
}

int64_t
u32_strcasestr (u32char_t* str,
                u32char_t* sstr)
{
    #define __U8LCASE(X) utf8proc_tolower(X)
    if (str && sstr) {
        int64_t i, j, N, M;
        N = _intrnl_u32strlen(str);
        M = _intrnl_u32strlen(sstr);

        int64_t* d = ucm_zmalloc (M * sizeof(int64_t));

        // prefix function
        for (i = 1, j = 0; i < M; i++)
        {
            while ( j > 0 && __U8LCASE( sstr[j] ) != __U8LCASE( sstr[i] ))
                j = d [j-1];
            if (__U8LCASE( sstr [j] ) == __U8LCASE( sstr [i] ))
                j++;
            d [i] = j;
        }

        // Search function
        for (i = 0, j = 0; i < N; i++)
        {
            while ( j > 0 && __U8LCASE( sstr [j] ) != __U8LCASE( str [i] ))
                j = d [j - 1];
            if ( __U8LCASE( sstr [j] ) == __U8LCASE( str [i] ))
                j++;
            if ( j == M) {
                ucm_free(d);
                return i - j + 1;
            }
        }
        ucm_free (d);
    }
    return -1;
    #undef __U8LCASE
}
