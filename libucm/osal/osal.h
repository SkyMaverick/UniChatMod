#pragma once

#include <stdint.h>
#include <stdbool.h>

#include <uv.h>
#include <assert.h>

#include "osal-dynlib.h"
#include "osal-memory.h"
#include "osal-threading.h"
#include "osal-system.h"

#if (defined(__GNUC__) && (__GNUC__ > 2 && __GNUC_MINOR__ > 0)) || \
    (defined(__INTEL_COMPILER) && __INTEL_COMPILER >= 800) 
    
    #define __likely(x) __builtin_expect(!!(x), 1)
    #define __ulikely(x) __builtin_expect(!!(x), 0)
#else
    #define __likely(x) (x)
    #define __ulikely(x) (x)
#endif

#if defined(__WIN32__) || defined(_WIN32) || defined(WIN32)
    #  define AL_OS_WIN32
#elif defined(_WIN64) || defined(_M_X64) || defined(_M_AMD64)
    #  define AL_OS_WIN64
#elif defined(__linux) || defined(__linux__)
    #  define AL_OS_LINUX
#elif defined(__ANDROID__)
    #  define AL_OS_ANDROID
#elif defined(__MSYS__)
    #  define AL_OS_MSYS
#elif defined(__CYGWIN__)
    #  define AL_OS_CYGWIN
#elif defined(__APPLE__) && (defined(__GNUC__) || defined(__xlC__) || defined(__xlc__))
    #  define AL_OS_DARWIN
#elif defined(__FreeBSD__)
    #  define AL_OS_FREEBSD
    #  define AL_OS_BSD4
#elif defined(__DragonFly__)
    #  define AL_OS_DRAGONFLY
    #  define AL_OS_BSD4
#elif defined(__NetBSD__)
    #  define AL_OS_NETBSD
    #  define AL_OS_BSD4
#elif defined(__OpenBSD__)
    #  define AL_OS_OPENBSD
    #  define AL_OS_BSD4
#else
    #error "Unsupported build for this platform. \n Please add support this platform in OSAL functionality."
#endif

#if defined (AL_OS_MSYS) || defined (AL_OS_CYGWIN)
    #define AL_OS_WINEMULATOR
#endif

#if defined (AL_OS_WIN32) || defined (AL_OS_WIN64)
    #define AL_OS_WINDOWS
#endif

#if defined (AL_OS_BSD4)    ||  \
    defined (AL_OS_DARWIN)  ||  \
    defined (AL_OS_LINUX)   ||  \
    defined (AL_OS_ANDROID) ||  \
    defined (AL_OS_CYGWIN)  ||  \
    defined (AL_OS_MSYS)        \
    
    #define AL_OS_POSIX
#endif
