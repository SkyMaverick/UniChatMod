#pragma once

enum
{
    UCM_FLAG_EVENT_MULTYCAST = 1 << 0,
    UCM_FLAG_EVENT_CRYPTO    = 1 << 1,
    UCM_FLAG_EVENT_ALERT     = 1 << 2
};

enum
{
    UCM_TYPE_EVENT_NORMAL = 0,
    UCM_TYPE_EVENT_FILE   = 1,
    UCM_TYPE_EVENT_ROOM   = 2,
    UCM_TYPE_EVENT_STATUS = 3,
    UCM_TYPE_EVENT_EVENT  = 4
};

typedef struct
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
} ucm_event_t;
