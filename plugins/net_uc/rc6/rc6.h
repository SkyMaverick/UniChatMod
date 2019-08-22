/**
 * @file rc6.h
 * @brief RC6-32/20 block cipher
 *
 * @section License
 *
 * Copyright (C) 2010-2018 Oryx Embedded SARL. All rights reserved.
 *
 * This file is part of CycloneCrypto Open.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * @author Oryx Embedded SARL (www.oryx-embedded.com)
 * @author Alexander Smirnov (Skymaverick) 2018
 * @version 1.8.6
 **/

#ifndef _UCM_CUSTOM_RC6_
#define _UCM_CUSTOM_RC6_
// RC6 block size
#define RC6_BLOCK_SIZE 16
// Maximum length of the encryption key in bytes
#define RC6_MAX_KEY_SIZE 256 /* 2048 bits */
// Number of rounds
#define RC6_NB_ROUNDS 16

// Common interface for encryption algorithms
#define RC6_CIPHER_ALGO (&rc6CipherAlgo)

// C++ guard
#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/**
 * @brief RC6 algorithm context
 **/

enum { NO_ERROR = 0, ERROR_INVALID_KEY_LENGTH = 1 << 1 };

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

#define ROR32(a, n) (((a) >> (n)) | ((a) << (32 - (n))))
#define ROL32(a, n) (((a) << (n)) | ((a) >> (32 - (n))))

#define LOAD32LE(p)                                                                  \
    (((uint32_t)(((uint8_t*)(p))[0]) << 0) | ((uint32_t)(((uint8_t*)(p))[1]) << 8) | \
     ((uint32_t)(((uint8_t*)(p))[2]) << 16) | ((uint32_t)(((uint8_t*)(p))[3]) << 24))

#define STORE32LE(a, p)                                                                                   \
    ((uint8_t*)(p))[0] = ((uint32_t)(a) >> 0) & 0xFFU, ((uint8_t*)(p))[1] = ((uint32_t)(a) >> 8) & 0xFFU, \
    ((uint8_t*)(p))[2] = ((uint32_t)(a) >> 16) & 0xFFU, ((uint8_t*)(p))[3] = ((uint32_t)(a) >> 24) & 0xFFU

typedef struct {
    uint32_t l[RC6_MAX_KEY_SIZE / 4];
    uint32_t s[2 * RC6_NB_ROUNDS + 4];
} Rc6Context;

// RC6 related functions
int rc6Init(Rc6Context* context, const uint8_t* key, size_t keyLen);
void rc6EncryptBlock(Rc6Context* context, const uint8_t* input, uint8_t* output);
void rc6DecryptBlock(Rc6Context* context, const uint8_t* input, uint8_t* output);

// C++ guard
#ifdef __cplusplus
}
#endif

#endif
