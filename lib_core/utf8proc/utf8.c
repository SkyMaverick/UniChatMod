#include "utf8.h"
#include "alloc.h"

#include <string.h>

#define UNUSED(X) (void)(X)
#define CHAR_SIZE sizeof(uchar_t)

static int
uc_valid (uchar_t* str)
{
    UNUSED (str);
    return 0;
}

static size_t
uec_strlen (uechar_t* str)
{
    size_t  n, count = 0;
    uchar_t code;
    while ((n = utf8proc_iterate(str, -1, &code)) > 0)
        count++;
    return count;
}

static ssize_t
uec2str (uechar_t** ue_str)
{
    size_t buf_len   = sizeof(ucm_string_t) + uec_strlen (*ue_str) +1 * CHAR_SIZE;
    ucm_string_t* buf = (buf_len > 0) ? ucm_zmalloc( buf_len ) : NULL;

    if (buf) {
        uchar_t ch;
        while ( utf8proc_iterate (*ue_str, -1, &ch) > 0)
            buf->data [ buf->lenght++ ] = ch;

        if ( utf8proc_normalize_utf32 ( buf->data, buf->lenght, UTF8PROC_STRIPCC
                                                               |UTF8PROC_COMPOSE
                                                               |UTF8PROC_STABLE) > 0 ) 
        {
            if (!ucm_realloc ((void**)ue_str, buf_len)) {
                memcpy (*ue_str, buf, buf_len);
                ucm_free (buf);
                return ((ucm_string_t*)(*ue_str))->lenght;
            }
        }
        ucm_free (buf);
    }
    return -1;
}

static ssize_t
str2uec (ucm_string_t** u_str)
{
    utf8proc_ssize_t bytes = 0;
    size_t string_len = (*u_str)->lenght + CHAR_SIZE;

    if ( (bytes = utf8proc_reencode ( (*u_str)->data, string_len, UTF8PROC_STRIPCC
                                                                 |UTF8PROC_COMPOSE
                                                                 |UTF8PROC_STABLE) ) > 0 )
    {
        memmove (*u_str, (*u_str)->data, bytes);
        return ( realloc (*u_str, bytes) ) ? -1 : bytes;
    }
    return -1;
}
