#include <stdlib.h>
#include <errno.h>

#ifdef __linux__
    #include <sys/prctl.h>
#endif

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "ucm.h"
#include "alloc.h"
#include "network.h"
#include "net_legacy.h"

static void
__select_func (void* ctx)
{
#ifdef __linux__
    trace_dbg ("%s\n", "Use thread name");
    prctl (PR_SET_NAME, "ucl_select_loop",0,0,0,0);
#endif
    fd_set rfds;
    int retval;
    struct timeval tv;

    ucl_connection_t* Con = (ucl_connection_t*) ctx;

    if (Con) {
        Con->proto.lan_status = UCM_FLAG_NETSTAT_LISTEN;

        tv.tv_sec  = 0;
        tv.tv_usec = 50000;

        FD_ZERO (&rfds);
        while (Con->proto.lan_status != UCM_FLAG_NETSTAT_OFF) {
            FD_SET (Con->ip.socket, &rfds);

            retval = select(Con->ip.socket + 1, &rfds, NULL, NULL, &tv);
            if (retval < 0) {
                trace_err ("%s\n", "Don't select socket" );
                Con->proto.lan_status = UCM_FLAG_NETSTAT_OFF;
                return;
            }
            if (retval > 0) {
                //TODO accept connection
            }
        }
    }
}

static int
_create_socket (ucl_connection_t* Con,
                uint32_t          ip,
                uint16_t          port)
{
    if ( (Con->ip.socket = socket (AF_INET, SOCK_DGRAM, 0) ) < 0 ) {
        trace_err ("%s - %d:%d. %s\n", "Don't create socket", ip, port, strerror(errno));
        return UCM_RET_NOOBJECT;
    }

    int flags = fcntl (Con->ip.socket, F_GETFL,0);
    if (flags == -1) {
        trace_err ("%s\n", "fcntl F_GETFL fail");
        return UCM_RET_EXCEPTION;
    }
    if (fcntl (Con->ip.socket, F_SETFL, flags | O_NONBLOCK) < 0){
        trace_err ("%s\n", "fcntl F_SETFL fail");
        return UCM_RET_EXCEPTION;
    }

    Con->ip.addr.sin_family         = AF_INET;
    Con->ip.addr.sin_port           = htons (port);
    Con->ip.addr.sin_addr.s_addr    = htonl (ip);

    if ( bind ( Con->ip.socket,
                (struct sockaddr*) &Con->ip.addr,
                sizeof(Con->ip.addr) ) < 0 )
    {
        trace_err ("%s - %d:%d. %s\n", "Don't bind socket", ip, port, strerror(errno));
        return UCM_RET_EXCEPTION;
    }
    return UCM_RET_SUCCESS;
}

ucm_conptr_t
ucl_connect ()
{
    ucl_connection_t* con_s = ucm_zmalloc (sizeof(ucl_connection_t));
    if (con_s) {
        // TODO get connection propeties in DB
        uint32_t ip = 0;
        uint16_t port = 0;

        if (_create_socket (con_s, ip, port) == UCM_RET_SUCCESS) {
            con_s->tid_select = app->thread_create (__select_func, (void*) con_s);
            if (con_s->tid_select <= 0) {
                trace_err ("%s : %s\n", "Don't create select thread", strerror(errno));
                ucm_free_null (con_s);
            };
            trace_dbg ("%s : %zu\n", "Create select thread", con_s->tid_select);
        } else  {
            ucm_free_null (con_s);
        }
        con_s->proto.net_status = UCM_FLAG_NETSTAT_ON;
    }
    return (uintptr_t) con_s;
}

int
ucl_disconnect (ucm_conptr_t* Con)
{
    if (Con && *Con > 0) {
        // TODO release connection
        ucl_connection_t* uCon = (ucl_connection_t*)(*Con);

        uCon->proto.net_status = UCM_FLAG_NETSTAT_OFF;
        app->thread_join (uCon->tid_select);

        ucm_free (uCon);
        *Con = 0;
    }
    return UCM_RET_SUCCESS;
}
