project ('mdbx', 'c',
         version: '0.3.1.0',
         license: 'ReOpenLDAP',
         default_options:[
            'c_std=gnu11',
            'warning_level=2',
            'b_lto=true',
            'b_colorout=always'
         ],
         meson_version: '>=0.51')

cc = meson.get_compiler('c')
ccid = cc.get_id()
osid = target_machine.system()

if (ccid == 'msvc') and (cc.version().split('.')[0].to_int() < 19)
    error ('MSVC compiler @0@ is too old for building MDBX. At least \'Microsoft Visual Studio 2015\' is required.'.format(cc.version()))
endif

app_echo = find_program ('echo', required: false)
app_cat  = find_program ('cat', required: false)
app_ld   = find_program ('ld', required: false)

if (get_option('MDBX_USE_GIT'))
    app_git  = find_program ('git', required: true)
endif

app_py = import('python').find_installation()
if app_py.found()
    message ('Python version found: @0@'.format(run_command (app_py, '--version').stdout().strip()))
else
    error ('Python NOT found')
endif

c_args   = []
ld_args  = []
c_defs   = []
ld_deps  = []
mdbx_deps = []

if not cc.has_function('pow')
    m_dep = find_library ('m', required: false)
    if (m_dep.found()) and (cc.has_function('pow', prefix: '#include <math.h>'))
        mdbx_deps += m_dep
    else
        error ('No libm found for math support')
    endif
endif

mdbx_deps += dependency ('threads')

if (ccid == 'msvc') 
    c_args += '''
        /DWIN32
        /D_WINDOWS
        /WX
    '''.split()
    ld_args += '''
    '''.split()
    ld_deps += '''
        ntdll
        kernel32
        user32
        gdi32
        winspool
        shell32
        ole32
        oleaut32
        uuid
        comdlg32
        advapi32
    '''.split()
else
    c_args += '''
        -Wno-unknown-pragmas
        -fexceptions
        -funwind-tables
        -fno-omit-frame-pointer
        -fno-common
        -ggdb
        -fvisibility=hidden
        -march=native
        -Og
        -Ominimal
        -ffunction-sections -fdata-sections
        -ffast-math
        -fopenmp
    '''.split()
endif

foreach item : c_args
    if cc.has_argument (item)
        add_project_arguments (item, language: 'c')
    endif
endforeach
foreach item : ld_args
    if cc.has_link_argument (item)
        add_project_link_arguments (item, language: 'c')
    endif
endforeach
foreach item : c_defs
    add_project_arguments (item, language: 'c')
endforeach

foreach item : ld_deps
    syslib = cc.find_library (item, required: false)
    if syslib.found()
        mdbx_deps += syslib
    endif
endforeach

cfg = configuration_data()

cfg.set   ('LTO_ENABLED', get_option('b_lto'))
cfg.set   ('ENABLE_GPROF', not ( get_option('b_pgo') == 'off') )
cfg.set   ('ENABLE_GCOV', get_option('b_coverage'))
cfg.set   ('ENABLE_ASAN', not ( get_option('b_sanitize') == 'none') )
cfg.set10 ('MDBX_TXN_CHECKPID', get_option ('MDBX_TXN_CHECKPID'))
cfg.set10 ('MDBX_TXN_CHECKOWNER', get_option ('MDBX_TXN_CHECKOWNER'))
cfg.set10 ('MDBX_BUILD_SHARED_LIBRARY', get_option ('MDBX_BUILD_SHARED_LIBRARY'))
cfg.set10 ('MDBX_AVOID_CRT', (osid == 'windows')
                             and (not meson.is_subproject())
                             and get_option('MDBX_AVOID_CRT'))
cfg.set10 ('MDBX_CONFIG_MANUAL_TLS_CALLBACK', (osid == 'windows')
                                              and get_option ('MDBX_BUILD_SHARED_LIBRARY')
                                              and get_option ('MDBX_CONFIG_MANUAL_TLS_CALLBACK'))
cfg.set10 ('MDBX_USE_ROBUST', not (osid == 'windows')
                              and get_option('MDBX_USE_ROBUST'))
cfg.set10 ('MDBX_USE_OFDLOCKS', not (osid == 'windows')
                                and get_option('MDBX_USE_OFDLOCKS'))

cfg.set10 ('MDBX_OSX_SPEED_INSTEADOF_DURABILITY', (osid == 'darwin'))
cfg.set_quoted   ('MDBX_BUILD_TARGET',osid)
cfg.set_quoted   ('MDBX_BUILD_COMPILER', ccid)
cfg.set_quoted   ('MDBX_BUILD_FLAGS','undefined')

subdir ('src')
