project ('ucm', 'c',
         version: '0.1.1',
         license: 'Zlib',
         default_options:[
         ],
         subproject_dir: 'deps',
         meson_version: '>=0.49')

message ('Build configuration - @0@'.format(get_option('buildtype')))

cc = meson.get_compiler('c')
ccid = cc.get_id()
osid = target_machine.system()

ucm_cflags  = []
ucm_ldflags = []
ucm_defs    = []

# --- compiler preferences ----------
if get_option('buildtype').startswith('debug')
    if ccid == 'msvc'
        ucm_defs += '/DDEBUG'
    else
        ucm_defs += '-DDEBUG'
    endif
endif
if get_option('enable_traces')
    if ccid == 'msvc'
        ucm_defs += '/DENABLE_TRACE_MESSAGES'
    else
        ucm_defs += '-DENABLE_TRACE_MESSAGES'
    endif
endif
if get_option('create_bundle')
    if ccid == 'msvc'
        ucm_defs += '/DENABLE_BUNDLE'
    else
        ucm_defs += '-DENABLE_BUNDLE'
    endif
endif
if get_option('create_with_hidden_api')
    if ccid == 'msvc'
        ucm_defs += '/DLIBUCM_EXPORTS'
    else
        ucm_defs += '-DLIBUCM_EXPORTS'
    endif
endif
if get_option('valgrind_support')
    if not (ccid == 'msvc')
        ucm_defs += '-DENABLE_VALGRIND'
    endif
endif

if (ccid == 'gcc') or (ccid == 'clang')
    ucm_defs += '-D_XOPEN_SOURCE=700'
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
lib_inc = [
    '.',
    '.'/'libucm',
    '.'/'libucm'/'include',
    '.'/'libucm'/'osal'
]

libs_rpath = ':'.join ([
    '$ORIGIN'/'.',
    '$ORIGIN'/'libs', '.',
    '$ORIGIN'/'mods', '.',
    '$ORIGIN'/'..'/'libs', '.',
    '$ORIGIN'/'..'/'mods', '.'
])

# --- install paths resolve ----------
install_path_root = get_option('prefix') / meson.project_name ()

install_path_mods = install_path_root / get_option ('path_modules_lookup')
install_path_libs = install_path_root / get_option ('path_libs_lookup')
install_path_docs = install_path_root / get_option ('path_docs_lookup')
install_path_devs = install_path_root / get_option ('path_devels_lookup')

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
        uv_lib = subproject('uv', default_options: ['='.join (['path_install', install_path_libs])])
        system_library_uv = uv_lib.get_variable('uv_dep')

        shared_headers += uv_lib.get_variable('uv_header')
        system_dependencies += system_library_uv.partial_dependency(
                    includes: true
                )
        if ccid == 'msvc'
            ucm_defs += '/DENABLE_CUSTOM_LIBS'
        else
            ucm_defs += '-DENABLE_CUSTOM_LIBS'
        endif
    endif
else
    uv_lib = subproject('uv', default_options: ['='.join (['path_install', install_path_libs])])
    system_library_uv = uv_lib.get_variable('uv_dep')

    shared_headers += uv_lib.get_variable('uv_header')
    system_dependencies += system_library_uv.partial_dependency(
                    includes: true
                )

    if ccid == 'msvc'
        ucm_defs += '/DENABLE_CUSTOM_LIBS'
    else
        ucm_defs += '-DENABLE_CUSTOM_LIBS'
    endif
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
    add_project_arguments (item, language: 'c')
endforeach

# --- include headers paths ----------
libucm_inc = include_directories (lib_inc)

# --- walk in subdirs ----------
subdir('libucm')

if get_option('create_with_plugins')
    subdir('plugins')
endif

if get_option('create_with_gui')
    subdir('gui')
endif

if get_option('create_with_tests')
    subdir('tests')
endif

# -- post-targets install ------
install_headers (shared_headers,
                 install_dir: install_path_devs)

