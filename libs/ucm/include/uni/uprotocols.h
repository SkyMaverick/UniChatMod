#pragma once

typedef uintptr_t ucm_conptr_t;

enum { UCM_FLAG_NETSTAT_OFF = 0, UCM_FLAG_NETSTAT_ON = 1, UCM_FLAG_NETSTAT_LISTEN = 2 };

enum {
    UCM_FLAG_NET_IAMSERVERMODE = 1 << 0,
    UCM_FLAG_NET_FILETRANSFER = 1 << 1,
    UCM_FLAG_NET_GROUP = 1 << 2,
    UCM_FLAG_NET_CRYPT = 1 << 3,
    UCM_FLAG_NET_COMPRESS = 1 << 4,
    UCM_FLAG_NET_BROADCAST = 1 << 5,
    UCM_FLAG_NET_MULTYSESSION = 1 << 6,
    UCM_FLAG_NET_NOSERVER = 1 << 7
};

typedef struct {
    ucm_plugin_t core;
    uint32_t flags;

    const char** (*get_interface)(void);
    ucm_conptr_t (*connect)(uintptr_t ctx);
    UCM_RET (*disconnect)(ucm_conptr_t* cptr);
    int (*get_status)(void);
} ucm_plugproto_t;
