#include <string.h>

#include "ucm.h"
#include "alloc.h"
#include "uconv.h"
#include "utf8proc.h"

u32char_t*
u8to_u32 (u8char_t* str)
{
    u32char_t chr;
    size_t block_size = 0x400 * U32CHAR_SIZE;
    size_t chr_cnt    = 0;

    u32char_t* ret = ucm_zmalloc (block_size);
    if ( ret ) {
        while ( utf8proc_iterate(str, -1, &chr) > 0) {
            if (chr_cnt > block_size - 1) {
                block_size *= 2;
                u32char_t* tmp_block = ucm_zmalloc (block_size);
                if (tmp_block){
                    memcpy(tmp_block, ret, chr_cnt * U32CHAR_SIZE);
                } else {
                    free(ret);
                    break;
                }
            }
            chr_cnt++;
        }
    }
    return ret;
}

u8char_t*
u32to_u8 (u32char_t* str)
{

}

void
u8to_u32conv (u8char_t* str)
{

}

void
u32to_u8conv (u32char_t* str)
{

}
// void
// u16to_u32 (u16char_t* str)
// {
// 
// }
// 
// void
// u32to_u16 (u32char_t* str)
// {
// 
// }
