#pragma once

/* **************************************************
    Operation System Abstraction Layer (OSAL)
   ************************************************** */

// *** OPERATING SYSTEM MACRO ****************************************

#if defined(__WIN32__) || defined(_WIN32) || defined(WIN32)
    #define UCM_OS_WIN32
#elif defined(_WIN64) || defined(_M_X64) || defined(_M_AMD64)
    #define UCM_OS_WIN64
#elif defined(__linux) || defined(__linux__)
    #define UCM_OS_LINUX
#elif defined(__ANDROID__)
    #define UCM_OS_ANDROID
#elif defined(__MSYS__)
    #define UCM_OS_MSYS
#elif defined(__CYGWIN__)
    #define UCM_OS_CYGWIN
#elif defined(__APPLE__) && (defined(__GNUC__) || defined(__xlC__) || defined(__xlc__))
    #define UCM_OS_DARWIN
#elif defined(__FreeBSD__)
    #define UCM_OS_FREEBSD
    #define UCM_OS_BSD4
#elif defined(__DragonFly__)
    #define UCM_OS_DRAGONFLY
    #define UCM_OS_BSD4
#elif defined(__NetBSD__)
    #define UCM_OS_NETBSD
    #define UCM_OS_BSD4
#elif defined(__OpenBSD__)
    #define UCM_OS_OPENBSD
    #define UCM_OS_BSD4
#else
    #error \
      "Unsupported build for this platform. \n Please add support this platform in OSAL functionality."
#endif

#if defined(UCM_OS_MSYS) || defined(UCM_OS_CYGWIN)
    #define UCM_OS_WINEMULATOR
#endif

#if defined(UCM_OS_WIN32) || defined(UCM_OS_WIN64)
    #define UCM_OS_WINDOWS
#endif

#if defined(UCM_OS_BSD4) || defined(UCM_OS_DARWIN) || defined(UCM_OS_LINUX) || \
  defined(UCM_OS_ANDROID) || defined(UCM_OS_CYGWIN) || defined(UCM_OS_MSYS)

    #define UCM_OS_POSIX
#endif

// *** COMPILATOR MACRO ****************************************

#if defined(_MSC_VER)
    #define UCM_CC_MSVC
    #if defined(__INTEL_COMPILER)
        #define UCM_CC_INTEL
    #endif
    #if defined(__clang__)
        #define UCM_CC_CLANG
    #endif
#elif defined(__GNUC__)
    #define UCM_CC_GNU
    #if defined(__MINGW32__)
        #define UCM_CC_MINGW
    #endif
    #if defined(__INTEL_COMPILER)
        #define UCM_CC_INTEL
    #endif
    #if defined(__clang__)
        #define UCM_CC_CLANG
    #endif
    #if defined(_CRAYC)
        #define UCM_CC_CRAY
    #endif
#elif defined(__SUNPRO_C) || defined(__SUNPRO_CC)
    #define UCM_CC_SUN
#elif defined(__xlc__) || defined(__xlC__)
    #define UCM_CC_XLC
#elif defined(__HP_cc) || defined(__HP_aCC)
    #define UCM_CC_HP
#elif defined(__DECC) || defined(__DECCXX)
    #define UCM_CC_DEC
#elif (defined(__sgi) || defined(sgi)) && \
  (defined(_COMPILER_VERSION) || defined(_SGI_COMPILER_VERSION))
    #define UCM_CC_MIPS
#elif defined(__USLC__) && defined(__SCO_VERSION__)
    #define UCM_CC_USLC
#elif defined(__WATCOMC__)
    #define UCM_CC_WATCOM
#elif defined(__BORLANDC__)
    #define UCM_CC_BORLAND
#elif defined(__INTEL_COMPILER)
    #define UCM_CC_INTEL
#elif defined(__PGI)
    #define UCM_CC_PGI
#elif defined(_CRAYC)
    #define UCM_CC_CRAY
#endif

#if (defined(__GNUC__) && (__GNUC__ > 2 && __GNUC_MINOR__ > 0)) || \
  (defined(__INTEL_COMPILER) && __INTEL_COMPILER >= 800)

    #define __likely(x) __builtin_expect(!!(x), 1)
    #define __ulikely(x) __builtin_expect(!!(x), 0)
#else
    #define __likely(x) (x)
    #define __ulikely(x) (x)
#endif

#if defined(__clang__)
    #define UCM_FUNC_DEPRECATED(x) __attribute__((deprecated(x)))
#elif defined(__GNUC__)
    #if !defined(__GNUC_PREREQ)
        #if defined(__GNUC_MINOR__)
            #define __GNUC_PREREQ(maj, min) \
                ((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
        #else
            #define __GNUC_PREREQ(maj, min) 0
        #endif
    #endif

    #if __GNUC_PREREQ(4, 5)
        #define UCM_FUNC_DEPRECATED(x) __attribute__((deprecated(x)))
    #else
        #define UCM_FUNC_DEPRECATED(x) __attribute__((deprecated))
    #endif
#else
    #define UCM_FUNC_DEPRECATED(x)
#endif

#ifndef UCM_API_LEVEL
    #define UCM_API_LEVEL UCM_API_MINOR_VER
#endif

#if (ENABLE_WARN_DEPRECATED && UCM_API_LEVEL >= 1)
    #define DEPRECATED_01 UCM_FUNC_DEPRECATED("This function deprecated in API ver. >= 0.1")
#else
    #define DEPRECATED_01
#endif

// === IMPORT / EXPORT ====================

#ifndef __has_attribute
    #define __has_attribute(x) (0)
#endif

#ifndef __ucm_export
    #if defined(UCM_OS_WINDOWS) || defined(UCM_OS_WINEMULATOR)
        #if defined(UCM_CC_GNU) || __has_attribute(dllexport)
            #define __ucm_export __attribute__((dllexport))
        #elif defined(UCM_CC_MSVC)
            #define __ucm_export __declspec(dllexport)
        #else
            #define __ucm_export
        #endif
    #elif defined(UCM_CC_GNU) || __has_attribute(visibility)
        #define __ucm_export __attribute__((visibility("default")))
    #else
        #define __ucm_export
    #endif
#endif /* __ucm_export */

#ifndef __ucm_import
    #if defined(UCM_OS_WINDOWS) || defined(UCM_OS_WINEMULATOR)
        #if defined(UCM_CC_GNU) || __has_attribute(dllimport)
            #define __ucm_import __attribute__((dllimport))
        #elif defined(UCM_CC_MSVC)
            #define __ucm_import __declspec(dllimport)
        #else
            #define __ucm_import
        #endif
    #else
        #define __ucm_import
    #endif
#endif /* __ucm_import */

#if defined(LIBUCM_EXPORTS)
    #define LIBUCM_API __ucm_export
#elif defined(LIBMDBX_IMPORTS)
    #define LIBUCM_API __ucm_import
#else
    #define LIBUCM_API
#endif
