tuiapp_name = 'ucmc'
tuiapp_src = files('''
    app.c
    app.h
'''.split()) + shared_headers

if not (target_machine.system() == 'windows')
    tuiapp_dep = [
        system_library_dynload,
        system_dependencies
    ]
else
    tuiapp_src += files ('''
        getopt_win.h
    '''.split())

    tuiapp_dep = [
        system_dependencies
    ]
endif

tuiapp = executable (tuiapp_name,
             tuiapp_src,
             include_directories: libucm_inc,
             dependencies: tuiapp_dep,
             build_rpath: '$ORIGIN/.',
             install_rpath: '$ORIGIN/.',
             install: true,
             install_dir: install_path_root
         )
