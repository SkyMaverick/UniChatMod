#ifndef _NCURSES_MAIN_H_
#define _NCURSES_MAIN_H_

#include "ucm.h"

UCM_RET
start_curses_app (void* ctx);

void
finish_curses_app (int sig);

#endif
