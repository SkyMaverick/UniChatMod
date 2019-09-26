#pragma once

enum
{
    UCM_FLAG_MSG_MULTYCAST = 1 << 0,
    UCM_FLAG_MSG_CRYPTO    = 1 << 1,
    UCM_FLAG_MSG_ALERT     = 1 << 2
};

enum
{
    UCM_TYPE_MSG_NORMAL = 0,
    UCM_TYPE_MSG_FILE   = 1,
    UCM_TYPE_MSG_ROOM   = 2,
    UCM_TYPE_MSG_STATUS = 3,
    UCM_TYPE_MSG_EVENT  = 4
};

typedef struct ucm_msg_s
{
    ucm_object_t oid;

    uint8_t type;
    // TODO sender info
    time_t time;
    uint32_t flags;
    struct
    {
        size_t size;
        uint8_t blob[1];
    } data;
} ucm_message_t;
