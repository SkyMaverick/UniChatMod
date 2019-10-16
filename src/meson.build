tuiapp_name = 'ucmc'
tuiapp_src = files('''
    app.c
    app.h
    argcmd.c
    argcmd.h
    osal.h
    osal.c
'''.split()) + shared_headers

tuiapp_dep = [libucm_depends]
if not (target_machine.system() == 'windows')
    tuiapp_dep += [
        system_library_dynload,
        system_dependencies
    ]
else
    tuiapp_src += files ('''
        getopt_win.h
    '''.split())

    tuiapp_dep += [
        system_dependencies
    ]
endif

tuiapp = executable (tuiapp_name,
             tuiapp_src,
             dependencies: tuiapp_dep,
             build_rpath: libs_rpath,
             install_rpath: libs_rpath,
             install: true,
             install_dir: install_path_app
         )
