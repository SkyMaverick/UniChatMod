#include "utf8.h"
#include "alloc.h"

#include <string.h>

#define UNUSED(X) (void)(X)
#define CHAR_SIZE sizeof(uchar_t)

static int
uc_valid (uchar_t* str,
          ssize_t   lenght)
{
    while (lenght >= 0)
        if ( !utf8proc_codepoint_valid (str[ lenght-- ]) )
            return 0;
    return 1;
}

static size_t
uec_strlen (uechar_t* str)
{
    uchar_t code;
    size_t  count = 0;
    while ( utf8proc_iterate ( str, -1, &code ) > 0) count++;
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
            if ( !ucm_realloc ((void**)ue_str, buf_len) ) {
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

// ===================================

size_t 
ucm_strlen (ucm_string_t* string)
{
    return string->lenght - 1;
}

ucm_string_t*
ucm_strdup (ucm_string_t* string)
{
    size_t ret_size = sizeof(ucm_string_t)          // struct size
                      + string->lenght * CHAR_SIZE; // string lenght
    ucm_string_t* ret = ucm_malloc ( ret_size );
    if (ret)
        memcpy (ret, string, ret_size );
    return ret;
}

void
ucm_strcat (ucm_string_t* result,
            ucm_string_t** cat)
{
    size_t new_size = sizeof(ucm_string_t)          // struct size
                      + result->lenght * CHAR_SIZE  // 1-st string lenght
                      + (*cat)->lenght * CHAR_SIZE  // 2-nd string lenght
                      - CHAR_SIZE;                  // - one /0 terminate
    if ( !ucm_realloc ( (void*)(&result), new_size) ) {
        memcpy ( &( result->data [ result->lenght - 1 ] ), (*cat)->data, (*cat)->lenght);
        result->lenght = new_size;
        ucm_free_null (*cat);
    }
}

int8_t
ucm_strcmp (ucm_string_t* left,
            ucm_string_t* right)
{
    if (left->lenght != right->lenght)
        return ( left->lenght > right->lenght ) ? -1 : 1;
    for (size_t i=0; i < left->lenght; i++)
        if ( left->data[ i ] != right->data[ i ] )
            return ( left->data[ i ] > right->data[ i ] ) ? -1 : 1;
    return 0;
}

int8_t
ucm_strcasecmp (ucm_string_t* left,
                ucm_string_t* right)
{
    if (left->lenght != right->lenght)
        return ( left->lenght > right->lenght ) ? -1 : 1;
    for (size_t i=0; i < left->lenght; i++) 
    {
        uchar_t a = utf8proc_tolower ( left->data[ i ] );
        uchar_t b = utf8proc_tolower ( right->data[ i ] );

        if ( a != b )
            return ( a > b ) ? -1 : 1;
    }
    return 0;
}

int8_t
ucm_strncmp (ucm_string_t* left,
             ucm_string_t* right,
             size_t        num)
{

}

void
ucm_strcpy (ucm_string_t* dest,
            ucm_string_t* source)
{

}

void
ucm_strncpy (ucm_string_t* dest,
             ucm_string_t* source,
             size_t        num)
{

}
