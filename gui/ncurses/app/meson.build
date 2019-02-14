tuiapp_name = 'ucmc'
tuiapp_src = files('''
    app.c
    app.h
'''.split()) + shared_headers

tuiapp_dep = [
    system_library_dynload,
    system_dependencies
]

tuiapp = executable (tuiapp_name,
             tuiapp_src,
             include_directories: libucm_inc,
             dependencies: tuiapp_dep,
             build_rpath: '$ORIGIN/.',
             install_rpath: '$ORIGIN/.',
             install: true,
             install_dir: install_path_root
         )
