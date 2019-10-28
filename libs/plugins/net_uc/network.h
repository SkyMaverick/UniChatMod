#ifndef _UCM_NETWORK_LEGACY_H_
#define _UCM_NETWORK_LEGACY_H_

#include "ucm.h"

#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>

typedef struct {
    struct {
        int socket;
        struct sockaddr_in addr;
    } ip;

    struct {
        int is_server;
        int net_status;
        int lan_status;
    } proto;

    uintptr_t tid_select;
    uintptr_t mtx;
} ucl_connection_t;

ucm_conptr_t
ucl_connect();

int
ucl_disconnect(ucm_conptr_t* cptr);

#endif
