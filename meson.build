project ('ucm', 'c',
         version: '0.1.1',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11',
         ],
         subproject_dir: 'deps',
         meson_version: '>=0.46')

cc = meson.get_compiler('c')

# --- compiler preferences ----------
if get_option('buildtype').startswith('debug')
    add_global_arguments ('-DDEBUG', language: 'c')
endif
if get_option('enable_traces')
    add_global_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
endif
if get_option('build_bundle')
    add_global_arguments ('-DENABLE_BUNDLE', language: 'c')
    add_global_arguments ('-DENABLE_BASE_BUNDLE', language: 'c')
endif
if get_option('build_with_hidden_api')
    add_global_arguments ('-DLIBUCM_EXPORTS', language: 'c')
endif

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
    join_paths ('.',''),
    join_paths ('.', 'lib_core'),
    join_paths ('.', 'lib_core', 'osal'),
    join_paths ('.', 'lib_core', 'include'),
]

libs_rpath = ':'.join ([
    join_paths ('$ORIGIN','.'),
    join_paths ('$ORIGIN', '..', 'libs', '.'),
    join_paths ('$ORIGIN', '..', 'mods', '.')
])

# --- include headers paths ----------
lib_core_inc = include_directories (lib_inc)

# --- install paths resolve ----------
if get_option ('build_bundle')
    install_path_root = join_paths (meson.source_root(), 'bundle', meson.project_name ())
    install_path_mods = join_paths (install_path_root, get_option ('path_modules_lookup'))
    install_path_libs = join_paths (install_path_root, get_option ('path_libs_lookup'))
    install_path_docs = join_paths (install_path_root, get_option ('path_docs_lookup'))
    install_path_devs = join_paths (install_path_root, get_option ('path_devels_lookup'))
endif

# --- resolve some popularity dependencies ----------

if target_machine.system() == 'windows'
    system_library_threads = cc.find_library ('pthread', required: false)
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
