#pragma once

#include "app.h"

#include "curses.h"
#include "panel.h"

#ifdef UCM_OS_WINDOWS
    #include "windefs.h"
#else
    #include "lindefs.h"
#endif

uintptr_t
curses_start(void);

void*
curses_dispatch(void* ctx);

void
curses_finish(void);
