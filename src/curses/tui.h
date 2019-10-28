#include "app.h"

#ifdef UCM_OS_WiNDOWS
    #include "windefs.h"
#else
    #include "lindefs.h"
#endif

UCM_RET
curses_start(void);

void
curses_finish(void);
