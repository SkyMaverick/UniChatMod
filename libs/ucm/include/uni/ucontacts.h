#pragma once

typedef uint64_t HCONTACT;

typedef struct ucm_cont_s {
    ucm_object_t oid; // ucm system object ID

    HCONTACT cid; // global contact ID
    struct {
        char* name;
    } info;
    // TODO
} ucm_contact_t;

// Users ----------------------
enum {
    UCM_USER_STATUS_INACTIVE = 0,
    UCM_USER_STATUS_ACTIVE = 1 << 0,
    UCM_USER_STATUS_ABSENT = 1 << 1,
    UCM_USER_STATUS_NAVIALABLE = 1 << 2,
    UCM_USER_STATUS_BUSY = 1 << 3,
    UCM_USER_STATUS_DNTDISTURB = 1 << 4,
    UCM_USER_STATUS_MAYTALK = 1 << 5,
    UCM_USER_STATUS_INVISIBLE = 1 << 6,
};
