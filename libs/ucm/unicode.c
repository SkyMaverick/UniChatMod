#include "unicode.h"

#include "api.h"
#include "ucm.h"

#include <stdarg.h>
#include <string.h>

#define MIN2(A, B) ((A) < (B) ? (A) : (B))

#define MIN3(A, B, C) (((A) < (B)) ? ((A) < (C)) ? (A) : (C) : ((B) < (C)) ? (B) : (C))

// ==========================================
//     INTERNAL INLINE FUNCTIONS
// ==========================================

// WARNING! without NULL-terminated symbol
static inline size_t
_intrnl_u32strlen(ucm_wstr_t str) {
    size_t count = 0;
    if (str) {
        for (; *str; str++, count++) {
        };
    }
    return count;
}

static inline void
_intrnl_u32strcpy(ucm_wstr_t dest, ucm_wstr_t src, ucm_wchr_t echr) {
    size_t i;
    if (dest && src) {
        for (i = 0; src[i] != 0; i++)
            dest[i] = src[i];
        dest[i] = echr;
    }
}

static inline void
_intrnl_u32strncpy(ucm_wstr_t dest, ucm_wstr_t src, ucm_wchr_t echr, size_t num) {
    size_t i;
    if (dest && src) {
        for (i = 0; i < num && src[i] != 0; i++)
            dest[i] = src[i];
        dest[i] = echr;
    }
}

// ==========================================
//     CONVERT FUNCTIONS
// ==========================================

/*  Decode UTF8 buffer to allocated USC4 buffer
    str - UTF8 buffer
    str_len - UTF8 string lenght (maybe 0 for NULLTERM string)
    ret - pointer for decoded USC4 buffer */

int64_t
u8_decode_ucs4(ucm_str_t str, const size_t str_len, ucm_wstr_t* ret) {
    ssize_t buf_len = 0;
    const int infinite = (str_len > 0) ? 0 : UTF8PROC_NULLTERM;

    if (str) {
        if (str_len <= 0) {
            // calculate assumed buffer size
            ucm_str_t tmp = str;
            while (tmp)
                tmp++, buf_len++; // bytes in string
            buf_len /= 2;
        } else
            buf_len = str_len;
        *ret = UniAPI->sys.zmalloc((buf_len + 1) * UCMSZ_WCHR);
        if (*ret) {
            for (;;) {
                ssize_t dec_len = 0;
                dec_len =
                    utf8proc_decompose((utf8proc_uint8_t*)str, str_len, (utf8proc_int32_t*)*ret,
                                       buf_len, UTF8PROC_DECOMPOSE | UTF8PROC_STABLE | infinite);
                if (dec_len < 0) {
                    ucm_free_null(*ret);
                    return dec_len;
                }

                if (dec_len == buf_len)
                    return dec_len;

                if (dec_len < buf_len) {
                    if (UniAPI->sys.realloc((void**)ret, (dec_len + 1) * UCMSZ_WCHR)) {
                        ucm_free_null(*ret);
                        return 0;
                    } else
                        return dec_len;
                }

                if (UniAPI->sys.realloc((void**)ret, (dec_len + 1) * UCMSZ_WCHR)) {
                    ucm_free_null(*ret);
                    return 0;
                }
            }
        }
    }
    return buf_len;
}

int64_t
ucs4_encode_u8(ucm_wstr_t str, const size_t str_len, ucm_str_t* ret) {
    size_t enc_len = 0;
    if (str && *ret) {
        size_t buf_size = 0;
        if (str_len > 0) {
            buf_size = str_len;
        } else {
            while (*str)
                str++, buf_size++;
        }
        *ret = (ucm_str_t)ucm_strdup2(str);
        enc_len = utf8proc_reencode((utf8proc_int32_t*)*ret, buf_size,
                                    UTF8PROC_STRIPCC | UTF8PROC_COMPOSE | UTF8PROC_STABLE);
        if (UniAPI->sys.realloc((void**)ret, (enc_len + 1) * UCMSZ_CHR)) {
            ucm_free_null(*ret);
            return 0;
        }
    }
    return enc_len;
}

// ==========================================
//     BASIC FUNCTIONS
// ==========================================

size_t
ucm_strlen(ucm_wstr_t str) {
    return _intrnl_u32strlen(str);
}

ucm_wstr_t
ucm_strdup2(ucm_wstr_t str) {
    ucm_wstr_t buf = NULL;

    if (str) {
        size_t buf_size = (ucm_strlen(str) + 1) * UCMSZ_WCHR;
        buf = UniAPI->sys.zmalloc(buf_size);
        if (buf)
            memcpy(buf, str, buf_size);
    }
    return buf;
}

// ==========================================
//     COMPARE FUNCTIONS
// ==========================================

int
ucm_strcmp(ucm_wstr_t lstr, ucm_wstr_t rstr) {
    while (*lstr && (*lstr == *rstr))
        lstr++, rstr++;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
ucm_strcasecmp(ucm_wstr_t lstr, ucm_wstr_t rstr) {
    while (*lstr && (utf8proc_tolower(*lstr) == utf8proc_tolower(*rstr)))
        lstr++, rstr++;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
ucm_strncmp(ucm_wstr_t lstr, ucm_wstr_t rstr, size_t num) {
    if (num == 0)
        return 0;
    while (*lstr && (*lstr == *rstr) && num)
        lstr++, rstr++, num--;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

int
ucm_strncasecmp(ucm_wstr_t lstr, ucm_wstr_t rstr, size_t num) {
    if (num == 0)
        return 0;
    while (*lstr && (utf8proc_tolower(*lstr) == utf8proc_tolower(*rstr)) && num)
        lstr++, rstr++, num--;
    return (*lstr > *rstr) - (*lstr < *rstr);
}

// ==========================================
//     CASEFOLD FUNCTIONS
// ==========================================

void
ucm_strupcase(ucm_wstr_t str) {
    while (*str)
        utf8proc_toupper(*str++);
}

void
ucm_strlowcase(ucm_wstr_t str) {
    while (*str)
        utf8proc_tolower(*str++);
}

// ==========================================
//     STRING COPY FUNCTIONS
// ==========================================

void
ucm_strcpy(ucm_wstr_t dest, ucm_wstr_t src) {
    _intrnl_u32strcpy(dest, src, 0);
}

void
ucm_strncpy(ucm_wstr_t dest, ucm_wstr_t src, size_t num) {
    _intrnl_u32strncpy(dest, src, 0, num);
}

// ==========================================
//     STRING CONCAT FUNCTIONS
// ==========================================

void
ucm_strcat(ucm_wstr_t dest, ucm_wstr_t src) {
    size_t dest_size = _intrnl_u32strlen(dest);
    _intrnl_u32strcpy(dest + dest_size, src, 0);
}

void
ucm_strncat(ucm_wstr_t dest, ucm_wstr_t src, size_t num) {
    size_t dest_size = _intrnl_u32strlen(dest);
    _intrnl_u32strncpy(dest + dest_size, src, 0, num);
}

void
ucm_vstrcat(ucm_wstr_t dest, unsigned num, va_list va) {
    size_t i = _intrnl_u32strlen(dest) + 1;
    size_t j;

    while (num--) {
        ucm_wstr_t tmp_str = va_arg(va, ucm_wstr_t);
        if (tmp_str) {
            for (j = 0; tmp_str[j] != 0; i++, j++)
                dest[i] = tmp_str[j];
            dest[i] = 0;
        }
    }
}

void
ucm_mstrcat(ucm_wstr_t dest, unsigned num, ...) {
    va_list strs;
    va_start(strs, num);
    ucm_vstrcat(dest, num, strs);
    va_end(strs);
}

// ==========================================
//     STRING GET CHAR POSITION FUNCTIONS
// ==========================================

ucm_wstr_t
ucm_strchr(ucm_wstr_t str, ucm_wchr_t chr) {
    if (!str)
        return NULL;

    size_t i;
    for (i = 0; str[i] != chr; i++) {
    };
    return &str[i];
}

ucm_wstr_t
ucm_strrchr(ucm_wstr_t str, ucm_wchr_t chr) {
    if (!str)
        return NULL;

    size_t i, size = _intrnl_u32strlen(str);

    for (i = size; str[i] != chr; i++) {
    };
    return &str[i];
}

// ==========================================
//     STRING JOIN FUNCTIONS
// ==========================================

ucm_wstr_t
ucm_strjoin(ucm_wstr_t str1, ucm_wstr_t str2) {
    ucm_wstr_t result = NULL;
    if (str1 && str2) {
        size_t str1_len = _intrnl_u32strlen(str1);
        size_t str2_len = _intrnl_u32strlen(str2);

        result = UniAPI->sys.zmalloc((str1_len + str2_len + 1) * UCMSZ_WCHR);

        if (result) {
            _intrnl_u32strcpy(result, str1, 0);
            _intrnl_u32strcpy(result + str1_len, str2, 0);
        }
    }
    return result;
}

ucm_wstr_t
ucm_vstrjoin(size_t num, va_list va) {
    ucm_wstr_t buf = NULL;
    size_t buf_len = 0;
    va_list tmp_va;

    // calculate buffer size
    va_copy(tmp_va, va);
    for (size_t i = 0; i < num; i++) {
        ucm_wstr_t tmp_buf = va_arg(tmp_va, ucm_wstr_t);
        if (tmp_buf && *tmp_buf)
            buf_len += _intrnl_u32strlen(tmp_buf);
    }
    va_end(tmp_va);

    // copy to buffer
    buf = UniAPI->sys.zmalloc((buf_len + 1) * UCMSZ_WCHR);
    if (buf) {
        ucm_wstr_t p = buf;
        for (size_t i = 0; i < num; i++) {
            ucm_wstr_t tmp_buf = va_arg(va, ucm_wstr_t);
            if (tmp_buf && *tmp_buf) {
                _intrnl_u32strcpy(p, tmp_buf, 0);
                p += _intrnl_u32strlen(tmp_buf);
            }
        }
    }
    return buf;
}

ucm_wstr_t
ucm_mstrjoin(size_t num, ...) {
    ucm_wstr_t ret = NULL;

    va_list strs;
    va_start(strs, num);

    ret = ucm_vstrjoin(num, strs);

    va_end(strs);
    return ret;
}

// ==========================================
//     STRING JOIN WITH BROKEN CHAR FUNCTIONS
// ==========================================

ucm_wstr_t
ucm_strbrkjoin(ucm_wstr_t str1, ucm_wstr_t str2, ucm_wchr_t brk) {
    ucm_wstr_t result = NULL;
    if (str1 && str2) {
        size_t str1_len = _intrnl_u32strlen(str1);
        size_t str2_len = _intrnl_u32strlen(str2);

        result = UniAPI->sys.zmalloc((str1_len + str2_len + 1) * UCMSZ_WCHR);
        if (result) {
            _intrnl_u32strcpy(result, str1, brk);
            _intrnl_u32strcpy(result + str1_len + 1, str2, 0);
        }
    }
    return result;
}

ucm_wstr_t
ucm_vstrbrkjoin(size_t num, ucm_wchr_t brk, va_list va) {
    ucm_wstr_t buf = NULL;
    size_t buf_len = 0;
    va_list tmp_va;

    // calculate buffer size
    va_copy(tmp_va, va);
    for (size_t i = 0; i < num; i++) {
        ucm_wstr_t tmp_buf = va_arg(tmp_va, ucm_wstr_t);
        if (tmp_buf && *tmp_buf)
            buf_len += _intrnl_u32strlen(tmp_buf) + 1;
    }
    va_end(tmp_va);

    // copy to buffer
    buf = UniAPI->sys.zmalloc(buf_len * UCMSZ_WCHR);
    if (buf) {
        ucm_wstr_t p = buf;
        for (size_t i = 0; i < num; i++) {
            ucm_wstr_t tmp_buf = va_arg(va, ucm_wstr_t);
            if (tmp_buf && *tmp_buf) {
                _intrnl_u32strcpy(p, tmp_buf, brk);
                p += _intrnl_u32strlen(tmp_buf) + 1;
            }
        }
    }
    return buf;
}

ucm_wstr_t
ucm_mstrbrkjoin(ucm_wchr_t brk, size_t num, ...) {
    ucm_wstr_t ret = NULL;

    va_list strs;
    va_start(strs, num);

    ret = ucm_vstrbrkjoin(num, brk, strs);

    va_end(strs);
    return ret;
}

// ==========================================
//      SUBSTRING SEARCH FUNCTIONS
// ==========================================

int64_t
ucm_strstr(ucm_wstr_t str, ucm_wstr_t sstr) {
    if (str && sstr) {
        size_t i, j, N, M;
        N = _intrnl_u32strlen(str);
        M = _intrnl_u32strlen(sstr);

        size_t* d = UniAPI->sys.zmalloc(M * sizeof(int64_t));

        // prefix function
        for (i = 1, j = 0; i < M; i++) {
            while (j > 0 && sstr[j] != sstr[i])
                j = d[j - 1];
            if (sstr[j] == sstr[i])
                j++;
            d[i] = j;
        }

        // Search function
        for (i = 0, j = 0; i < N; i++) {
            while (j > 0 && sstr[j] != str[i])
                j = d[j - 1];
            if (sstr[j] == str[i])
                j++;
            if (j == M) {
                UniAPI->sys.free(d);
                return i - j + 1;
            }
        }
        UniAPI->sys.free(d);
    }
    return -1;
}

int64_t
ucm_strcasestr(ucm_wstr_t str, ucm_wstr_t sstr) {
#define __U8LCASE(X) utf8proc_tolower(X)
    if (str && sstr) {
        size_t i, j, N, M;
        N = _intrnl_u32strlen(str);
        M = _intrnl_u32strlen(sstr);

        size_t* d = UniAPI->sys.zmalloc(M * sizeof(int64_t));

        // prefix function
        for (i = 1, j = 0; i < M; i++) {
            while (j > 0 && __U8LCASE(sstr[j]) != __U8LCASE(sstr[i]))
                j = d[j - 1];
            if (__U8LCASE(sstr[j]) == __U8LCASE(sstr[i]))
                j++;
            d[i] = j;
        }

        // Search function
        for (i = 0, j = 0; i < N; i++) {
            while (j > 0 && __U8LCASE(sstr[j]) != __U8LCASE(str[i]))
                j = d[j - 1];
            if (__U8LCASE(sstr[j]) == __U8LCASE(str[i]))
                j++;
            if (j == M) {
                UniAPI->sys.free(d);
                return i - j + 1;
            }
        }
        UniAPI->sys.free(d);
    }
    return -1;
#undef __U8LCASE
}
