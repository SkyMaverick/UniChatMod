project ('ucm', 'c',
         version: '0.1.1',
         license: 'Zlib',
         default_options:[
         ],
         subproject_dir: 'deps',
         meson_version: '>=0.49')

message ('Build configuration - @0@'.format(get_option('buildtype')))

cc = meson.get_compiler('c')

if (cc.get_id() == 'gcc') or (cc.get_id() == 'clang')
    # --- compiler preferences ----------
    if get_option('buildtype').startswith('debug')
        add_project_arguments ('-DDEBUG', language: 'c')
    endif
    if get_option('enable_traces')
        add_project_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
    endif
    if get_option('build_bundle')
        add_project_arguments ('-DENABLE_BUNDLE', language: 'c')
    endif
    if get_option('build_with_hidden_api')
        add_project_arguments ('-DLIBUCM_EXPORTS', language: 'c')
    endif
    if get_option('valgrind_support')
        add_project_arguments ('-DENABLE_VALGRIND', language: 'c')
    endif
    
    add_project_arguments ('-D_XOPEN_SOURCE=700', language: 'c')
    # --- compiler arguments ----------
    cc_args = '''
        -std=gnu11
        -Wall
    '''.split()
    
    cc_link_args = '''
    '''.split()
    
    if get_option('buildtype').startswith('debug')
        cc_args += '''
                    -O0
                    -g
                  '''.split()
    elif get_option('buildtype').startswith('release')
        cc_args += '''
                    -O3
                    -s
                '''.split()
    endif
    
    if get_option('build_with_hidden_api')
        cc_args += '''
            -fvisibility=hidden
        '''.split()
    endif
elif cc.get_id() == 'msvc'
#    message ('Temporary use default case')

    cc_args = '''
    '''.split()

    cc_link_args = '''
    '''.split()
else
    error ('Don\'t support build with compiler: @0@. Use GCC on *nix or MSVC on Windows'.format(cc.get_id()))
endif
# --- Add all switches in project command line -----------
foreach arg : cc_args
    if cc.has_argument(arg)
        add_project_arguments(arg, language: 'c')
    endif
endforeach

foreach arg : cc_link_args
    if cc.has_link_argument (arg)
        add_project_link_arguments(arg, language: 'c')
    endif
endforeach

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

if target_machine.system() == 'windows'
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

        add_project_arguments('-DENABLE_CUSTOM_LIBS', language: 'c')
    endif
else
    uv_lib = subproject('uv', default_options: ['='.join (['path_install', install_path_libs])])
    system_library_uv = uv_lib.get_variable('uv_dep')
    
    shared_headers += uv_lib.get_variable('uv_header')
    system_dependencies += system_library_uv.partial_dependency(
                    includes: true
                )
    
    add_project_arguments('-DENABLE_CUSTOM_LIBS', language: 'c')
endif

# --- include headers paths ----------
libucm_inc = include_directories (lib_inc)

# --- walk in subdirs ----------
subdir('libucm')

if get_option('build_with_plugins')
    subdir('plugins')
endif

if get_option('build_with_gui')
    subdir('gui')
endif

if get_option('build_with_tests')
    subdir('tests')
endif

# -- post-targets install ------
install_headers (shared_headers,
                 install_dir: install_path_devs)

