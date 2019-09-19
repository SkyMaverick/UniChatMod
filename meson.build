project ('ucm', 'c',
         version: '0.1.2',
         license: 'Zlib',
         default_options:[
            'b_lto = true',
            'b_colorout = always'
         ],
         subproject_dir: 'deps',
         meson_version: '>=0.51')

message ('Build configuration - @0@'.format(get_option('buildtype')))

cc = meson.get_compiler('c')
ccid = cc.get_id()
osid = target_machine.system()

ucm_cflags  = []
ucm_ldflags = []
ucm_defs    = []

# --- compiler preferences ----------
if get_option('buildtype').startswith('debug')
    ucm_defs += 'DEBUG'
endif
if get_option('enable_traces')
    ucm_defs += 'ENABLE_TRACE_MESSAGES'
endif
if get_option('create_bundle')
    ucm_defs += 'ENABLE_BUNDLE'
endif
if get_option('create_with_hidden_api')
    ucm_defs += 'LIBUCM_EXPORTS'
endif
if get_option('valgrind_support')
    ucm_defs += 'ENABLE_VALGRIND'
endif

if (ccid == 'gcc') or (ccid == 'clang')
    ucm_defs += '_XOPEN_SOURCE=700'
    # --- compiler arguments ----------
    ucm_cflags += '''
        -std=gnu11
        -Wall
    '''.split()

    if get_option('buildtype').startswith('debug')
        ucm_cflags += '''
                    -O0
                    -g
                  '''.split()
    elif get_option('buildtype').startswith('release')
        ucm_cflags += '''
                    -O3
                    -s
                '''.split()
    endif

    if get_option('create_with_hidden_api')
        ucm_cflags += '''
            -fvisibility=hidden
        '''.split()
    endif
elif ccid == 'msvc'
    if get_option('buildtype').startswith('debug')
        cc_args = '''
            /Zi
            /Ob0
            /Oy-
            /DEBUG
        '''.split()
    endif

else
    error ('Don\'t support build with compiler: @0@. Use GCC on *nix or MSVC on Windows'.format(cc.get_id()))
endif
# --- Add all switches in project command line -----------
libs_inc = [
    include_directories('.'),
    include_directories('.'/'libucm'),
    include_directories('.'/'libucm'/'include'),
    include_directories('.'/'libucm'/'osal')
]

libs_rpath = ':'.join ([
    '$ORIGIN'/'.',
    '$ORIGIN'/'lib', '.',
])

libs_rpath_plugin = ':'.join ([
    '$ORIGIN'/'.',
    '$ORIGIN'/'..'
])

# --- install paths resolve ----------
install_path_plugin = get_option('libdir') / 'plugins'

# --- resolve some popularity dependencies ----------

if osid == 'windows'
    system_library_dynload = cc.find_library ('dl', required: false)
else
    system_library_dynload = cc.find_library ('dl', required: true)
endif

system_dependencies = []
shared_headers = []

# add libuv dependency
if not get_option('custom_libs')
    system_library_uv = cc.find_library('uv', required: false)
    if not system_library_uv.found()
        cmake = import ('cmake')
        uv_lib = cmake.subproject('uv',
                 cmake_options: [
                     '='.join (['-DBUILD_TESTING','OFF']),
                     '='.join (['-DCMAKE_INSTALL_INCLUDEDIR',get_option('includedir')]),
                     '='.join (['-D CMAKE_INSTALL_LIBDIR',get_option('libdir')])
                 ])
        libs_inc += uv_lib.include_directories('uv')
        system_library_uv = uv_lib.dependency('uv')

        ucm_defs += 'ENABLE_CUSTOM_LIBS'
    endif
else
        cmake = import ('cmake')
        uv_lib = cmake.subproject('uv',
                 cmake_options: [
                     '='.join (['-DBUILD_TESTING','OFF']),
                     '='.join (['-DCMAKE_INSTALL_INCLUDEDIR',get_option('includedir')]),
                     '='.join (['-D CMAKE_INSTALL_LIBDIR',get_option('libdir')])
                 ])
        libs_inc += uv_lib.include_directories('uv')
        system_library_uv = uv_lib.dependency('uv')

        ucm_defs += 'ENABLE_CUSTOM_LIBS'
endif

foreach item : ucm_cflags
    if cc.has_argument (item)
        add_project_arguments (item, language: 'c')
    endif
endforeach
foreach item : ucm_ldflags
    if cc.has_link_argument (item)
        add_project_link_arguments (item, language: 'c')
    endif
endforeach
foreach item : ucm_defs
    if ccid == 'msvc'
        item = '/D'+item
    else
        item = '-D'+item
    endif
    add_project_arguments (item, language: 'c')
endforeach

# --- walk in subdirs ----------
subdir('libucm')

if get_option('create_with_plugins')
    subdir('plugins')
endif

if get_option('create_with_tests')
    subdir('tests')
endif

# -- post-targets install ------
install_headers (shared_headers
#                 install_dir: install_path_devs
                 )

