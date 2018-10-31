tuiapp_name = 'ucmc'
tuiapp_src = files('''
    app.c
    app.h
'''.split()) + core_header_c

tuiapp_dep = [
    cc.find_library ('dl'       , required: true),
]

if get_option('build_bundle')
    tuiapp = executable (tuiapp_name,
                 tuiapp_src,
                 include_directories: lib_core_inc,
                 dependencies: tuiapp_dep,
                 build_rpath: '$ORIGIN/.',
                 install_rpath: '$ORIGIN/.',
                 install: true,
                 install_dir: install_path_root
             )
else
    tuiapp = executable (tuiapp_name,
                 tuiapp_src,
                 include_directories: lib_core_inc,
                 dependencies: tuiapp_dep,
                 install: true,
                 install_dir: install_path_root
             )
endif
