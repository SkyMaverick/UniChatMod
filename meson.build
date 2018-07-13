project ('ucm', 'c',
         version: '0.0.1',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11'
         ],
         meson_version: '>=0.46')

cc = meson.get_compiler('c')

# --- compiler preferences ----------
if get_option('buildtype').startswith('debug')
    add_global_arguments ('-DDEBUG', language: 'c')
endif
if get_option('enable_traces')
    add_global_arguments ('-DENABLE_TRACE_MESSAGES', language: 'c')
endif

# --- include headers paths ----------
lib_core_inc = include_directories ('.', 'lib_core')

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
