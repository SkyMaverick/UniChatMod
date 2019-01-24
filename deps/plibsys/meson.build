project ('plibsys', 'c',
         version: '0.0.4-dev',
         license: 'MIT',
         default_options:[
         ],
         meson_version: '>=0.49')

cc = meson.get_compiler('c')

PLIBSYS_BUILD_STATIC    = true
PLIBSYS_TESTS           = false
PLIBSYS_COVERAGE        = false
PLIBSYS_VISIBILITY      = true

PLIBSYS_PLATFORM_CFLAGS  = []
PLIBSYS_PLATFORM_LDFLAGS = []
# ==================================================
#   CHECK SYSTEM
# ==================================================

PLIBSYS_SIZEOF_VOID_P       = cc.sizeof ('void*')
PLIBSYS_SIZEOF_SIZE_T       = cc.sizeof ('size_t')
PLIBSYS_SIZEOF_LONG         = cc.sizeof ('long')

# **** DETECT_C_COMPILER ***************************
meson_ccid = cc.get_id()
meson_osid = target_machine.system()
meson_archid = target_machine.cpu_family()

if meson_ccid == 'gcc'
    PLIBSYS_C_COMPILER = 'gcc'
elif meson_ccid == 'msvc'
    PLIBSYS_C_COMPILER = 'msvc'
else
    PLIBSYS_C_COMPILER = meson_ccid
endif
if meson_ccid == 'intel'
    PLIBSYS_C_COMPILER = 'icc'
endif

# Qnx, Openwacom and xl not detect meson. 
# Use crossbuild definition for build

# **** Detect os bits ******************************
if PLIBSYS_SIZEOF_VOID_P == 8
    PLIBSYS_ARCH_BITS = '64'
elif PLIBSYS_SIZEOF_VOID_P == 4
    PLIBSYS_ARCH_BITS = '32'
elif PLIBSYS_SIZEOF_VOID_P == 0
    PLIBSYS_ARCH_BITS = 'universal'
else
    PLIBSYS_ARCH_BITS = 'unknown'
endif

# **** Detect cpu arch *****************************
if meson_archid == 'x86_64'
    if meson.is_cross_build()
        if meson_archid.endswith('i*86')
            PLIBSYS_PROCESSOR_ARCH = 'x86'
        else
            PLIBSYS_PROCESSOR_ARCH = 'x64'
        endif
    else
        if PLIBSYS_ARCH_BITS == '32'
            PLIBSYS_PROCESSOR_ARCH = 'x86'
        elif PLIBSYS_ARCH_BITS == '64'
            PLIBSYS_PROCESSOR_ARCH = 'x64'
        else
            PLIBSYS_PROCESSOR_ARCH = PLIBSYS_ARCH_BITS
        endif
    endif
else
    PLIBSYS_PROCESSOR_ARCH = meson_archid
endif

# **** Set target OS id ****************************

PLIBSYS_TARGET_OS = meson_osid

if PLIBSYS_TARGET_OS == 'windows'
    PLIBSYS_TARGET_PLATFORM = 'win' + PLIBSYS_ARCH_BITS + '-' + PLIBSYS_C_COMPILER
else
    PLIBSYS_TARGET_PLATFORM = PLIBSYS_TARGET_OS + '-' + PLIBSYS_C_COMPILER
endif

# **** VA_COPY check func **************************
PLIBSYS_VA_COPY = ''
if cc.has_header ('stdarg.h')
    va_func_names = '''
        va_copy
        _va_copy
        __va_copy
    '''.split()
    foreach item : va_func_names
        if cc.has_function (item, prefix: '#include <stdarg.h>')
            PLIBSYS_VA_COPY = item
            break
        endif
    endforeach
else
    error ('Support for <stdarg.h> is required for target plarform, not found')
endif

# **** HIDDEN check func **************************
if PLIBSYS_VISIBILITY
    PLIBSYS_VISIBILITY_CFLAGS = ''
    PLIBSYS_VISIBILITY_LDFLAGS = ''

    if not ((meson_osid == 'windows') or (meson_osid == 'cygwin'))
        hidden_cflags = '''
            -fvisibility=hidden
            -qvisibility=hidden
            -Bhidden
        '''.split()
        foreach item : hidden_cflags
            if cc.has_argument (item)
                PLIBSYS_VISIBILITY_CFLAGS = item
                break
            endif
        endforeach
        
        hidden_ldflags = '''
            -xldscope=__hidden
        '''.split()
        foreach item : hidden_ldflags
            if cc.has_link_argument (item)
                PLIBSYS_VISIBILITY_LDFLAGS = item
                break
            endif
        endforeach
    endif
endif

subdir ('src')
