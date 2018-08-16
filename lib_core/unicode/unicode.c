#include "unicode.h"
#include "alloc.h"

#include <stdarg.h>
#include <string.h>

#define CHAR_SIZE sizeof(uchar_t)

#define MIN2(A,B) ((A) < (B) ? (A) : (B))

#define MIN3(A,B,C)(((A) < (B)) ? \
                    ((A) < (C)) ? (A) : (C) : \
                    ((B) < (C)) ? (B) : (C) )

// ==========================================
//     INTERNAL INLINE FUNCTIONS
// ==========================================

// WARNING! without NULL-terminated symbol
static inline size_t
_intrnl_u32strlen (uchar_t* str)
{
    size_t count = 0;
    if (str) {
        for (; *str; str++, count++){};
    }
    return count;
}

static inline void
_intrnl_u32strcpy (uchar_t* dest,
                   uchar_t* src,
                   uchar_t  echr)
{
    size_t i;
    if (dest && src) {
        for (i = 0; src[i] != 0; i++)
            dest [i] = src [i];
        dest [i] = echr;
    }
}

static inline void
_intrnl_u32strncpy (uchar_t* dest,
                    uchar_t* src,
                    uchar_t  echr,
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
ucm_strlen (uchar_t* str)
{
    return _intrnl_u32strlen (str);
}

uchar_t*
ucm_strdup (uchar_t* str)
{
    uchar_t* buf = NULL;

    if (str) {
        size_t buf_size = (ucm_strlen (str) + 1)  * CHAR_SIZE;
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
ucm_strcmp (uchar_t* lstr,
            uchar_t* rstr)
{
    while (*lstr && (*lstr == *rstr)) lstr++, rstr++;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
ucm_strcasecmp (uchar_t* lstr,
                uchar_t* rstr)
{
    while (*lstr && (utf8proc_tolower(*lstr) == utf8proc_tolower(*rstr)))
            lstr++, rstr++;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
ucm_strncmp (uchar_t* lstr,
             uchar_t* rstr,
             size_t num)
{
    if (num == 0) return 0;
    while (*lstr && (*lstr == *rstr) && num) lstr++, rstr++, num--;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
ucm_strncasecmp (uchar_t* lstr,
                 uchar_t* rstr,
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
ucm_strupcase (uchar_t* str)
{
    while(*str)
        utf8proc_toupper(*str++);
}

void
ucm_strlowcase (uchar_t* str)
{
    while(*str)
        utf8proc_tolower(*str++);
}

// ==========================================
//     STRING COPY FUNCTIONS
// ==========================================

void
ucm_strcpy (uchar_t* dest,
            uchar_t* src)
{
    _intrnl_u32strcpy (dest, src, 0);
}

void
ucm_strncpy (uchar_t* dest,
             uchar_t* src,
             size_t   num)
{
    _intrnl_u32strncpy (dest, src, 0, num);
}

// ==========================================
//     STRING CONCAT FUNCTIONS
// ==========================================

void
ucm_strcat (uchar_t* dest,
            uchar_t* src)
{
    size_t dest_size = _intrnl_u32strlen(dest);
    _intrnl_u32strcpy (dest + dest_size, src, 0);
}

void
ucm_strncat (uchar_t* dest,
             uchar_t* src,
             size_t   num)
{
    size_t dest_size = _intrnl_u32strlen(dest);
    _intrnl_u32strncpy (dest + dest_size, src, 0, num);
}

void
ucm_vstrcat (uchar_t* dest,
             unsigned num,
             va_list va)
{
    size_t i = _intrnl_u32strlen(dest) + 1;
    size_t j;

    while (num--) {
        uchar_t* tmp_str = va_arg (va, uchar_t*);
        if (tmp_str) {
            for (j = 0; tmp_str[j] != 0; i++, j++)
                dest [i] = tmp_str[j];
            dest [i] = 0;
        }
    }
}

void
ucm_mstrcat (uchar_t* dest,
             unsigned num,
             ...)
{
    va_list strs;
    va_start (strs, num);
    ucm_vstrcat (dest, num, strs);
    va_end (strs);
}

// ==========================================
//     STRING GET CHAR POSITION FUNCTIONS
// ==========================================

uchar_t*
ucm_strchr (uchar_t* str,
            uchar_t  chr)
{
    if (!str)
        return NULL;

    size_t i;
    for (i = 0; str[i] != chr; i++){};
    return &str[i];
}

uchar_t*
ucm_strrchr (uchar_t* str,
             uchar_t  chr)
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

uchar_t*
ucm_strjoin (uchar_t* str1,
             uchar_t* str2)
{
    uchar_t* result = NULL;
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

uchar_t*
ucm_vstrjoin (size_t   num,
              va_list  va)
{
    uchar_t* buf = NULL;
    size_t   buf_len  = 0;
    va_list  tmp_va;

    // calculate buffer size
    va_copy (tmp_va, va);
    for (size_t i = 0; i < num; i++) {
        uchar_t* tmp_buf = va_arg (tmp_va, uchar_t*);
        if (tmp_buf && *tmp_buf)
            buf_len += _intrnl_u32strlen(tmp_buf);
    }
    va_end (tmp_va);

    // copy to buffer
    buf = ucm_zmalloc ((buf_len + 1) * CHAR_SIZE);
    if ( buf ) {
        uchar_t* p = buf;
        for (size_t i = 0; i < num; i++) {
            uchar_t* tmp_buf = va_arg (va, uchar_t*);
            if (tmp_buf && *tmp_buf) {
                _intrnl_u32strcpy (p, tmp_buf, 0);
                p += _intrnl_u32strlen(tmp_buf);
            }
        }
    }
    return buf;
}

uchar_t*
ucm_mstrjoin (size_t   num,
              ...)
{
    uchar_t* ret = NULL;

    va_list strs;
    va_start (strs, num);

    ret = ucm_vstrjoin (num, strs);

    va_end (strs);
    return ret;
}

// ==========================================
//     STRING JOIN WITH BROKEN CHAR FUNCTIONS
// ==========================================

uchar_t*
ucm_strbrkjoin (uchar_t* str1,
                uchar_t* str2,
                uchar_t  brk)
{
    uchar_t* result = NULL;
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

uchar_t*
ucm_vstrbrkjoin (size_t   num,
                 uchar_t  brk,
                 va_list  va)
{
    uchar_t* buf = NULL;
    size_t   buf_len  = 0;
    va_list  tmp_va;

    // calculate buffer size
    va_copy (tmp_va, va);
    for (size_t i = 0; i < num; i++) {
        uchar_t* tmp_buf = va_arg (tmp_va, uchar_t*);
        if (tmp_buf && *tmp_buf)
            buf_len += _intrnl_u32strlen(tmp_buf)+1;
    }
    va_end (tmp_va);

    // copy to buffer
    buf = ucm_zmalloc (buf_len * CHAR_SIZE);
    if ( buf ) {
        uchar_t* p = buf;
        for (size_t i = 0; i < num; i++) {
            uchar_t* tmp_buf = va_arg (va, uchar_t*);
            if (tmp_buf && *tmp_buf) {
                _intrnl_u32strcpy (p, tmp_buf, brk);
                p += _intrnl_u32strlen(tmp_buf) + 1;
            }
        }
    }
    return buf;
}

uchar_t*
ucm_mstrbrkjoin (uchar_t  brk,
                 size_t   num,
                 ...)
{
    uchar_t* ret = NULL;

    va_list strs;
    va_start (strs, num);

    ret = ucm_vstrbrkjoin (num, brk, strs);

    va_end (strs);
    return ret;

}

// ==========================================
//      SUBSTRING SEARCH FUNCTIONS
// ==========================================

int64_t
ucm_strstr (uchar_t* str,
            uchar_t* sstr)
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
ucm_strcasestr (uchar_t* str,
                uchar_t* sstr)
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
