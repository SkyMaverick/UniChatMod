#pragma once

enum {
    UCM_FLAG_EVENT_MULTYCAST = 1 << 0,
    UCM_FLAG_EVENT_CRYPTO = 1 << 1,
    UCM_FLAG_EVENT_ALERT = 1 << 2
};

enum {
    UCM_TYPE_EVENT_NORMAL = 0,
    UCM_TYPE_EVENT_FILE = 1,
    UCM_TYPE_EVENT_ROOM = 2,
    UCM_TYPE_EVENT_STATUS = 3,
    UCM_TYPE_EVENT_EVENT = 4
};

typedef struct {
    ucm_object_t oid; /// internal object ID

    uint8_t type;  /// message type
    uint64_t size; ///
    uint32_t flags;

    struct {
        HCONTACT contact;
        // TODO protocol

        uint64_t time;
    } info;

} ucm_event_t;

typedef struct {
    ucm_event_t header;
    ucm_str_t data;
} ucm_message_t;

typedef void (*cb_evhook)(uint32_t eid, uintptr_t ev, uint32_t x1, uint32_t x2, void* ctx);
