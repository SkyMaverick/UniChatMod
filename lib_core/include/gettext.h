#ifndef _GETTEXT_H_
#define _GETTEXT_H_

#ifdef ENABLE_NLS
    #include <libintl.h>
    #define _(s) gettext(s)
#else
    #define _(s) (s)
#endif

#endif
