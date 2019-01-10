project ('ucm', 'c',
         version: '0.1.1',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11',
         ],
         subproject_dir: 'deps',
         meson_version: '>=0.49')

message ('Build configuration - @0@'.format(get_option('buildtype')))

cc = meson.get_compiler('c')

if (cc.get_id() == 'gcc') or (cc.get_id() == 'clang')
    # --- compiler preferences ----------
    if get_option('buildtype').startswith('debug')
        add_global_arguments ('-DDEBUG', language: 'c')
    endif
    if get_option('enable_traces')
        add_global_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
    endif
    if get_option('build_bundle')
        add_global_arguments ('-DENABLE_BUNDLE', language: 'c')
    endif
    if get_option('build_with_hidden_api')
        add_global_arguments ('-DLIBUCM_EXPORTS', language: 'c')
    endif
    if get_option('valgrind_support')
        add_global_arguments ('-DENABLE_VALGRIND', language: 'c')
    endif
    
    add_global_arguments ('-D_XOPEN_SOURCE=700', language: 'c')
    # --- compiler arguments ----------
    cc_args = '''
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
    '.'/'lib_core',
    '.'/'osal',
    '.'/'lib_core'/'include',
]

libs_rpath = ':'.join ([
    '$ORIGIN'/'.',
    '$ORIGIN'/'libs', '.',
    '$ORIGIN'/'mods', '.',
    '$ORIGIN'/'..'/'libs', '.',
    '$ORIGIN'/'..'/'mods', '.'
])

# --- include headers paths ----------
lib_core_inc = include_directories (lib_inc)

# --- install paths resolve ----------
install_path_root = get_option('prefix') / meson.project_name ()

install_path_mods = install_path_root / get_option ('path_modules_lookup')
install_path_libs = install_path_root / get_option ('path_libs_lookup')
install_path_docs = install_path_root / get_option ('path_docs_lookup')
install_path_devs = install_path_root / get_option ('path_devels_lookup')

# --- resolve some popularity dependencies ----------

if target_machine.system() == 'windows'
    system_library_threads = cc.find_library ('pthread', required: false)
    if not system_library_threads.found()
        ptwin_lib = subproject('pthreads4w', default_options: [ '='.join ( ['path_install', install_path_libs] )] )
        system_library_threads = ptwin_lib.get_variable('ptwin_dep')
    endif
    system_library_dynload = cc.find_library ('dl', required: false)
else
    system_library_threads = cc.find_library ('pthread', required: true)
    system_library_dynload = cc.find_library ('dl', required: true)
endif

# --- walk in subdirs ----------
subdir('lib_core')

if get_option('build_with_plugins')
    subdir('plugins')
endif

if get_option('build_with_gui')
    subdir('gui')
endif

if get_option('build_with_tests')
    subdir('tests')
endif
