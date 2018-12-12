#pragma once

#ifdef ENABLE_NLS
    #include <libintl.h>
    #define _(s) gettext(s)
#else
    #define _(s) (s)
#endif
