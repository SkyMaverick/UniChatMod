project ('ucm', 'c',
         version: '0.1.3',
         license: 'Zlib',
         default_options:[
            'c_std=gnu11',
            'warning_level=0',
            'b_lto=true',
            'b_colorout=always',
         ],
         subproject_dir: 'deps',
         meson_version: '>=0.52')

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
if get_option('custom_libs')
    ucm_defs += 'ENABLE_CUSTOM_LIBS'
endif

if (ccid == 'gcc') or (ccid == 'clang')
    ucm_defs += '_XOPEN_SOURCE=700'
    if get_option('create_with_hidden_api')
        ucm_cflags += '''
            -fvisibility=hidden
        '''.split()
    endif
elif ccid == 'msvc'
    if get_option('buildtype').startswith('debug')
        cc_args = '''
            /Ob0
            /Oy-
            /DEBUG
        '''.split()
    endif

else
    error ('Don\'t support build with compiler: @0@. Use GCC on *nix or MSVC on Windows'.format(cc.get_id()))
endif
# --- Add all switches in project command line -----------
libs_rpath = ':'.join ([
    '$ORIGIN'/'.',
    '$ORIGIN'/'lib', '.',
])

libs_rpath_plugin = ':'.join ([
    '$ORIGIN'/'.',
    '$ORIGIN'/'..'
])

# --- install paths resolve ----------
if meson.is_cross_build()
    is_portable = meson.get_cross_property ('portable')
    if is_portable
        install_path_app = get_option('prefix')
    else
        install_path_app = get_option('bindir')
    endif

    if osid == 'windows'
        install_path_libs = get_option('bindir')
    else
        install_path_libs = get_option('libdir')
    endif
else
    install_path_app = get_option('bindir')
    install_path_libs = get_option('libdir')
endif
install_path_plugin = install_path_libs / 'plugs'

# --- resolve some popularity dependencies ----------

if osid == 'windows'
    system_library_dynload = cc.find_library ('dl', required: false)
else
    system_library_dynload = cc.find_library ('dl', required: true)
endif

system_dependencies = []
shared_headers = []

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
subdir('libs')
if get_option ('create_with_startup')
    subdir ('src')
endif

if get_option('create_with_tests')
    subdir('tests')
endif

# -- post-targets install ------
install_headers (shared_headers)
