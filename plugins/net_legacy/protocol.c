#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#include "ucm.h"
#include "alloc.h"

#include "network.h"
#include "protocol.h"
#include "rc6/rc6.h"

#define SHUFFLE_BUFFER_SIZE 12
#define bswap(x) asm ("bswap %1" : "=r" ((x)) : "0" ((x)))

void
_shuffle_key (uint32_t  key,
              char*     passkey,
              size_t    pklen)
{
    uint8_t skey [SHUFFLE_BUFFER_SIZE] = {0xFF,0xFF,0xFF,0xFF,
                                          0xFF,0xFF,0xFF,0xFF,
                                          0xFF,0xFF,0xFF,0xFF};

    for (uint8_t i = 0; (i < pklen) && (i < SHUFFLE_BUFFER_SIZE); i++)
            skey[i] = passkey[i];

    register uint32_t eax asm ("%eax") = *((uint32_t*)(skey + 0));
    register uint32_t ebx asm ("%ebx") = *((uint32_t*)(skey + 4));
    register uint32_t ecx asm ("%ecx") = *((uint32_t*)(skey + 8));
    register uint32_t edx asm ("%edx") = key;

    if ( edx & ( 0x1U << 9 ))
        ecx ^= 0x2A289BE2;
    ebx ^= 0x32BEA272;

    if ( edx & ( 0x1U << 0 ))
        ebx >>= 3;
    bswap (eax);

    if ( edx & ( 0x1U << 30 ))
        eax ^= 0x2324BCA1;
    ebx ^= 0x323EBCDA;

    if ( edx & ( 0x1U << 1 ))
        ecx &= eax;
    ebx |= eax;

    if ( edx & ( 0x1U << 19 ))
        eax ^= 0x223B3212;
    ecx ^= 0x12BABAFA;

    if ( edx & ( 0x1U << 2 ))
        eax <<= 3;
    ecx >>= 7;

    if ( edx & ( 0x1U << 31 ))
        ebx ^= 0x123ABFE2;
    ecx ^= 0xF4F74234;

    if ( edx & ( 0x1U << 3 ))
        bswap (ebx);
    eax ^= ebx;

    if ( edx & ( 0x1U << 11 ))
        ebx ^= 0xBC872842;
    ecx ^= 0x01BFAE31;

    if ( edx & ( 0x1U << 4 ))
        eax = ~eax;
    bswap (ecx);

    if ( edx & ( 0x1U << 14 ))
        ecx ^= 0x294BACE7;
    ebx ^= 0x659B1EAB;

    if ( edx & ( 0x1U << 5 ))
        ebx <<= 3;
    ecx >>= 5;

    if ( edx & ( 0x1U << 12 ))
        eax ^= 0x12729563;
    ebx ^= 0x0BCD7B1E1;

    if ( edx & ( 0x1U << 6 ))
        ebx = ~ebx;
    ecx &= eax;

    if ( edx & ( 0x1U << 8 ))
        eax ^= 0x56FAA191;
    ebx ^= 0x9021F213;

    if ( edx & ( 0x1U << 7 ))
        eax <<= 4;
    ecx = ~ecx;

    if ( edx & ( 0x1U << 10 ))
        eax ^= 0x2854BCAF;
    ecx ^= 0x0AFE1323;

    if ( edx & ( 0x1U << 13 ))
        ecx ^= 0xA1A2A3A4;
    eax ^= 0x1BA5E81B;

    if ( edx & ( 0x1U << 15 ))
        eax ^= 0x120BC21A;
    ebx ^= 0xFBC12394;

    if ( edx & ( 0x1U << 16 ))
        ecx ^= 0x4792B2A4;
    ebx ^= 0xB5EE24E1;

    if ( edx & ( 0x1U << 17 ))
        ebx ^= 0xBCAEDFBA;
    ecx ^= 0x8B6AE2A1;

    if ( edx & ( 0x1U << 18 ))
        eax ^= 0x47BC1678;
    ebx ^= 0x4B4B6434;

    if ( edx & ( 0x1U << 20 ))
        ecx ^= 0x34289902;
    eax ^= 0xBAC34234;

    if ( edx & ( 0x1U << 21 ))
        eax ^= 0x0349BBAA;
    ebx ^= 0xAABB3423;

    if ( edx & ( 0x1U << 22 ))
        ebx ^= 0x34BCFA34;
    eax ^= 0xB2187A12;

    if ( edx & ( 0x1U << 23 ))
        eax ^= 0xC432123A;
    eax ^= 0x034BCCCC;

    if ( edx & ( 0x1U << 24 ))
        ebx ^= 0x10239054;
    ebx ^= 0xB234B213;

    if ( edx & ( 0x1U << 25 ))
        ecx ^= 0x56FAA191;
    ecx ^= 0x23B910F0;

    if ( edx & ( 0x1U << 26 ))
        eax ^= 0x0A0B0C0D;
    ebx ^= 0xD0E0B0F0;

    if ( edx & ( 0x1U << 27 ))
        ebx ^= 0x52321313;
    ecx ^= 0x65745234;

    if ( edx & ( 0x1U << 28 ))
        ecx ^= 0x123BCDE1;
    eax ^= 0x1239553B;

    if ( edx & ( 0x1U << 29 ))
        ebx ^= 0x12BC2312;
    ecx ^= 0x230BCDE1;

    *((uint32_t*)(skey + 0)) = eax;
    *((uint32_t*)(skey + 4)) = ebx;
    *((uint32_t*)(skey + 8)) = ecx;
}

ssize_t
send_data (void*  buffer,
           size_t lenght)
{
    uint32_t salt       = rand();
    uint8_t  long_flag  = 0;
    Rc6Context  rc6ctx;

    char* passkey = "";
    // TODO get passkey into database
    _shuffle_key (salt, passkey, strlen (passkey));
    rc6Init (&rc6ctx, (uint8_t*) passkey, 96);

    char*  adata = (char*) buffer;
    // TODO create message AData
    size_t asize = strlen (adata);
#ifdef ENABLE_SERVER_MODE
#endif
    if (asize >= 100) {
        long_flag = 1;
        // TODO compress
    }


    return 0;
}

UCM_RET
send_message (void*  buffer,
              size_t buffer_lenght)
{
    ssize_t res_size = 0;
    if ((create_packet (buffer)) > 0) {
        // TODO
    } else {
        // TODO send error trace message
    }
}

