project ('ucm', 'c',
         version: '0.1.1',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11'
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')

# --- compiler preferences ----------
add_global_arguments('-DUTF8PROC_STATIC', language: 'c')
if get_option('buildtype').startswith('debug')
    add_global_arguments ('-DDEBUG', language: 'c')
endif
if get_option('enable_traces')
    add_global_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
endif
if get_option('build_bundle')
    add_global_arguments ('-DENABLE_BUNDLE', language: 'c')
endif
if get_option('base_inbundle')
    add_global_arguments ('-DBASE_INBUNDLE', language: 'c')
endif

if get_option('build_with_hidden_api')
    add_global_arguments ('-DLIBUCM_EXPORTS', language: 'c')
endif

# --- compiler arguments ----------
c_args = [
    '-Wall'
]

if get_option('build_with_hidden_api')
    c_args += [
        '-fvisibility=hidden'
    ]
endif

if get_option('buildtype').startswith('debug')
    foreach arg : [
            '-O0',
            '-g'
        ]
        if cc.has_argument(arg)
            add_project_arguments(arg, language : 'c')
        endif
    endforeach
elif get_option('buildtype').startswith('release')
    foreach arg : [
            '-O3',
            '-s'
        ]
        if cc.has_argument(arg)
            add_project_arguments(arg, language : 'c')
        endif
    endforeach
endif

foreach arg : c_args
    if cc.has_argument(arg)
        add_project_arguments(arg, language : 'c')
    endif
endforeach

# --- include headers paths ----------
lib_core_inc = include_directories ('''
    ./
    ./lib_core
    ./lib_core/md5
    ./lib_core/unicode
'''.split())

# --- install paths resolve ----------
if get_option ('build_bundle')
    install_path_root = join_paths (meson.source_root(), 'bundle', meson.project_name())
    install_path_mods = join_paths (install_path_root, get_option ('path_modules_lookup'))
    install_path_docs = join_paths (install_path_root, get_option ('path_docs_lookup'))
    install_path_devs = join_paths (install_path_root, get_option ('path_devels_lookup'))
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
