#include <stdio.h>
#include <stdlib.h>
#include <uchar.h>
#include "../util.h"
#include "CUnit/Basic.h"

#include "unicode.h"
#include "api.h"

//  ************************************
//     STRLEN TESTS
//  ************************************

_TEST (strlen_lenght)
{
    u32char_t* A = U"0123456789";
    CU_ASSERT_EQUAL(ucm_strlen(A), 10);
}

_TEST (strlen_null)
{
    CU_ASSERT_EQUAL(ucm_strlen(NULL), 0);
}

_TEST (strlen_empty)
{
    CU_ASSERT_EQUAL(ucm_strlen(U""), 0);
}

//  ************************************
//     STRDUP TESTS
//  ************************************

_TEST (strdup_normal)
{
    u32char_t* A = U"01234_abcD";
    u32char_t* B = ucm_strdup (A);

    CU_ASSERT_PTR_NOT_NULL_FATAL(ucm_strdup(B));

    CU_ASSERT_EQUAL(ucm_strlen(A),
                    ucm_strlen(B));
    ucm_kfree (B);
}

_TEST (strdup_null)
{
    CU_ASSERT_PTR_NULL(ucm_strdup(NULL));
}

_TEST (strdup_empty)
{
    u32char_t* A = U"";
    u32char_t* B = ucm_strdup (A);
    CU_ASSERT_PTR_NOT_NULL_FATAL(B);
    CU_ASSERT_EQUAL(ucm_strlen(B), 0);
    ucm_kfree(B);
}

//  ************************************
//     STR(CASE)CMP TESTS
//  ************************************

_TEST (strcmp_equal)
{
    u32char_t* A = U"simple_test_string";
    CU_ASSERT_EQUAL(ucm_strcmp(A, A), 0);
    u32char_t* B = U"simple_TeST_string";
    CU_ASSERT_EQUAL(ucm_strcasecmp(A, B), 0);
}

_TEST (strcmp_notequal)
{
    u32char_t* A = U"simple_A_test";
    u32char_t* B = U"simple_b_test";
    CU_ASSERT_NOT_EQUAL(ucm_strcmp(A, B), 0);
    CU_ASSERT_NOT_EQUAL(ucm_strcasecmp(A, B), 0);
}

_TEST (strcmp_great)
{
    u32char_t* A = U"simple_A_test";
    u32char_t* B = U"simple_A_";
    CU_ASSERT_EQUAL(ucm_strcmp(A, B), 1);
    CU_ASSERT_EQUAL(ucm_strcasecmp(A, B), 1);
}

_TEST (strcmp_less)
{
    u32char_t* A = U"simple_A";
    u32char_t* B = U"simple_A_test";
    CU_ASSERT_EQUAL(ucm_strcmp(A, B), -1);
    CU_ASSERT_EQUAL(ucm_strcasecmp(A, B), -1);
}

//  ************************************
//     STRN(CASE)CMP TESTS
//  ************************************

_TEST (strncmp_equal)
{
    u32char_t* A = U"simple_test_string";
    u32char_t* B = U"simple_TeST_String";
    CU_ASSERT_EQUAL(ucm_strncmp(A, A, ucm_strlen(A)), 0);
    CU_ASSERT_EQUAL(ucm_strncasecmp(A, B, ucm_strlen(A)), 0);
}

_TEST (strncmp_notequal)
{
    u32char_t* A = U"simple_A_test";
    u32char_t* B = U"simple_b_test";
    CU_ASSERT_NOT_EQUAL(ucm_strncmp(A, B, ucm_strlen(A)), 0);
    CU_ASSERT_NOT_EQUAL(ucm_strncasecmp(A, B, ucm_strlen(A)), 0);
}

_TEST (strncmp_lenght)
{
    u32char_t* A = U"simple_A_test";
    u32char_t* B = U"simple_bad_test";
    CU_ASSERT_EQUAL(ucm_strncmp(A, B, 6), 0);
    CU_ASSERT_NOT_EQUAL(ucm_strncmp(A, B, 7), 0);
    CU_ASSERT_EQUAL(ucm_strncasecmp(A, B, 6), 0);
    CU_ASSERT_NOT_EQUAL(ucm_strncasecmp(A, B, 7), 0);
}

_TEST (strncmp_badlenght)
{
    u32char_t* A = U"simple_A_test";
    u32char_t* B = U"simple_bad_test";
    CU_ASSERT_NOT_EQUAL(ucm_strncmp(A, B, 1000), 0);
    CU_ASSERT_EQUAL(ucm_strncmp(A, B, 0), 0);
    CU_ASSERT_NOT_EQUAL(ucm_strncasecmp(A, B, 1000), 0);
    CU_ASSERT_EQUAL(ucm_strncasecmp(A, B, 0), 0);
}

_TEST (strncmp_great)
{
    u32char_t* A = U"simple_A_test";
    u32char_t* B = U"simple_A_";
    CU_ASSERT_EQUAL(ucm_strncmp(A, B, ucm_strlen(A)), 1);
    CU_ASSERT_EQUAL(ucm_strncasecmp(A, B, ucm_strlen(A)), 1);
}

_TEST (strncmp_less)
{
    u32char_t* A = U"simple_A";
    u32char_t* B = U"simple_A_test";
    CU_ASSERT_EQUAL(ucm_strncmp(A, B, ucm_strlen(A)), -1);
    CU_ASSERT_EQUAL(ucm_strncasecmp(A, B, ucm_strlen(A)), -1);
}

_TEST (strjoin_normal)
{
    u32char_t* A = U"abc";
    u32char_t* B = U"ABC";
    u32char_t* C = ucm_strjoin(A,B);
    CU_ASSERT_PTR_NOT_NULL_FATAL(C);
    CU_ASSERT_EQUAL(ucm_strcmp(C, U"abcABC"),0);
    ucm_kfree (C);
}

_TEST (strjoin_mv_normal)
{
    u32char_t* A = U"abc";
    u32char_t* B = U"ABC";
    u32char_t* C = ucm_mstrjoin(4, A, B, U"XYZ", U"xyz");
    CU_ASSERT_PTR_NOT_NULL_FATAL(C);
    CU_ASSERT_EQUAL(ucm_strcmp(C, U"abcABCXYZxyz"),0);
    ucm_kfree (C);
}

_TEST (strjoin_mv_null)
{
    u32char_t* A = U"abc";
    u32char_t* B = U"ABC";
    u32char_t* C = ucm_mstrjoin (4, A, B, NULL, U"xyz");
    CU_ASSERT_PTR_NOT_NULL_FATAL(C);
    CU_ASSERT_EQUAL(ucm_strcmp(C, U"abcABCxyz"),0);
    ucm_kfree (C);

    C = ucm_mstrjoin (4, A, B, NULL, U"");
    CU_ASSERT_EQUAL(ucm_strcmp(C, U"abcABC"),0);
    ucm_kfree (C);
}


_TEST (strbrkjoin_normal)
{
    u32char_t* A = U"abc";
    u32char_t* B = U"ABC";
    u32char_t* C = ucm_strbrkjoin (A,B,U'/');
    CU_ASSERT_PTR_NOT_NULL(C);
    CU_ASSERT_EQUAL(ucm_strcmp(C, U"abc/ABC"),0);
    ucm_kfree(C);
}

_TEST (strbrkjoin_mv_normal)
{
    u32char_t* A = U"abc";
    u32char_t* B = ucm_mstrbrkjoin (U'/' ,3 , A, U"ABC", U"xyz");
    CU_ASSERT_PTR_NOT_NULL(B);
    CU_ASSERT_EQUAL(ucm_strcmp(B, U"abc/ABC/xyz/"),0);
    ucm_kfree(B);
}

_TEST (strcat_normal)
{

}

_TEST (strcpy_normal)
{

}

_TEST (strchr_normal)
{

}

_TEST (strstr_normal) {
    u32char_t* A = U"aaacbbbCCBBBeee";
    CU_ASSERT_EQUAL(ucm_strstr(A, U"BBB"), 9);
    CU_ASSERT_EQUAL(ucm_strcasestr(A, U"BBB"), 4);
}

void runSuite (void) {
    // ... all startup functionality
    CU_pSuite suite=CUnitCreateSuite("Unicode functions tests");
    if(suite) {
        _ADD_TEST (suite, strlen_lenght);
        _ADD_TEST (suite, strlen_null);
        _ADD_TEST (suite, strlen_empty);

        _ADD_TEST (suite, strdup_normal);
        _ADD_TEST (suite, strdup_null);
        _ADD_TEST (suite, strdup_empty);

        _ADD_TEST (suite, strcmp_equal);
        _ADD_TEST (suite, strcmp_notequal);
        _ADD_TEST (suite, strcmp_great);
        _ADD_TEST (suite, strcmp_less);

        _ADD_TEST (suite, strncmp_equal);
        _ADD_TEST (suite, strncmp_notequal);
        _ADD_TEST (suite, strncmp_lenght);
        _ADD_TEST (suite, strncmp_badlenght);
        _ADD_TEST (suite, strncmp_great);
        _ADD_TEST (suite, strncmp_less);

        _ADD_TEST (suite, strjoin_normal);
        _ADD_TEST (suite, strjoin_mv_normal);
        _ADD_TEST (suite, strjoin_mv_null);

        _ADD_TEST (suite, strbrkjoin_normal);
        _ADD_TEST (suite, strbrkjoin_mv_normal);

        _ADD_TEST (suite, strcat_normal);

        _ADD_TEST (suite, strcpy_normal);

        _ADD_TEST (suite, strchr_normal);

        _ADD_TEST (suite, strstr_normal);
    }
    return;
}
