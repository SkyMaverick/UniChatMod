#pragma once

enum {
    UCM_DBVAL_FAIL = 0,
    UCM_DBVAL_U8 = 1,
    UCM_DBVAL_U16 = 2,
    UCM_DBVAL_U32 = 3,
    UCM_DBVAL_FLOAT = 4,
    UCM_DBVAL_DOUBLE = 5,
    UCM_DBVAL_ASCIIZ = 6,
    UCM_DBVAL_WIDESZ = 7,
    UCM_DBVAL_U8SZ = 8,
    UCM_DBVAL_BLOB = 9,
};

typedef struct {
    uint8_t type;
    union {
        uint8_t u8Val;
        uint16_t u16Val;
        uint32_t u32Val;

        float fVal;
        double dVal;

        struct {
            ucm_str_t sz8Val;
            size_t szSize;
        };
        struct {
            uint8_t* bVal;
            size_t bSize;
        };
    };
} ucm_dbval_t;

typedef struct _dbkey_s {
    HCONTACT contact;
    char* pid;
    char* setting;
} ucm_dbkey_t;

typedef struct {
    ucm_plugin_t core;

    // technical db functionality
    UCM_RET (*db_open)(void);
    UCM_RET (*db_check)(void);
    void (*db_flush)(bool force);
    UCM_RET (*db_close)(void);

    UCM_RET (*db_backup)(char* path);

    // hight-level API. Use app structures config with one API function
    ucm_dbval_t* (*get_setting)(HCONTACT contact, ucm_object_t* object, const char* setting,
                                ucm_dbval_t* defVal);
    UCM_RET(*set_setting)
    (HCONTACT contact, ucm_object_t* object, const char* setting, ucm_dbval_t* value);
    // TODO Events, contacts, logs iterators
} ucm_plugdb_t;
