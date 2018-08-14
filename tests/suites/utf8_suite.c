#include <stdio.h>
#include <stdlib.h>
#include <uchar.h>
#include "../util.h"
#include "CUnit/Basic.h"

#include "utf8.h"
#include "alloc.h"

_TEST(strlen_lenght) {
    uchar_t* A = U"0123456789";
    CU_ASSERT_EQUAL(ucm_strlen(A), 10);
}

_TEST(strlen_null) {
    CU_ASSERT_EQUAL(ucm_strlen(NULL), 0);
}

_TEST(strlen_empty) {
    CU_ASSERT_EQUAL(ucm_strlen(U""), 0);
}

_TEST(strdup_normal) {
    uchar_t* A = U"01234_abcD";
    uchar_t* B = ucm_strdup (A);

    CU_ASSERT_PTR_NOT_NULL(ucm_strdup(B));

    CU_ASSERT_EQUAL(ucm_strlen(A), 
                    ucm_strlen(B));
    ucm_free (B);
}

_TEST(strdup_null) {
    CU_ASSERT_PTR_NULL(ucm_strdup(NULL));   
}

_TEST(strdup_empty) {
    uchar_t* A = U"";
    uchar_t* B = ucm_strdup (A);
    CU_ASSERT_PTR_NOT_NULL(B);
    CU_ASSERT_EQUAL(ucm_strlen(B), 0);
    ucm_free(B);
}

_TEST(strcmp_equal) {
    uchar_t* A = U"simple_test_string";
    CU_ASSERT_EQUAL(ucm_strcmp(A, A), 0);
}

_TEST(strcmp_notequal) {
    uchar_t* A = U"simple_A_test";
    uchar_t* B = U"simple_B_test";
    CU_ASSERT_NOT_EQUAL(ucm_strcmp(A, B), 0);
}

_TEST(strcmp_great) {
    uchar_t* A = U"simple_A_test";
    uchar_t* B = U"simple_A_";
    CU_ASSERT_EQUAL(ucm_strcmp(A, B), 1);
}

_TEST(strcmp_less) {
    uchar_t* A = U"simple_A";
    uchar_t* B = U"simple_A_test";
    CU_ASSERT_EQUAL(ucm_strcmp(A, B), -1);
}

void runSuite (void) {
    // ... all startup functionality
    CU_pSuite suite=CUnitCreateSuite("Unicode functions tests");
    if(suite) {
        _ADD_TEST(suite, strlen_lenght);
        _ADD_TEST(suite, strlen_null);
        _ADD_TEST(suite, strlen_empty);

        _ADD_TEST(suite, strdup_normal);
        _ADD_TEST(suite, strdup_null);
        _ADD_TEST(suite, strdup_empty);

        _ADD_TEST(suite, strcmp_equal);
        _ADD_TEST(suite, strcmp_notequal);
        _ADD_TEST(suite, strcmp_great);
        _ADD_TEST(suite, strcmp_less);
    }
    return;
}
