#ifndef _UCM_NETWORK_LEGACY_H_
#define _UCM_NETWORK_LEGACY_H_

#include <sys/socket.h>
#include <sys/un.h>

#include "ucm.h"

enum {
    UCL_NETSTAT_OFF     = 0,
    UCL_NETSTAT_ON      = 1,
    UCL_NETSTAT_LISTEN  = 2
};

typedef struct {
    int socket;
    struct sockaddr_un address;

    int net_status;
    int lan_status;

    uintptr_t tid_listen;
} ucl_connection_t;

ucm_conptr_t
ucl_connect ();

int
ucl_disconnect (ucm_conptr_t* cptr);

#endif
