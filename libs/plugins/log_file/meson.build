plug_name = 'log_file'

stdlog_src = files('''
    log_file.c
'''.split()) + shared_headers

stdlog_dep = [
    system_dependencies,
    libucm_depends
]

lib_stdlog = shared_library(plug_name,
             stdlog_src,
             dependencies: stdlog_dep,
             build_rpath: libs_rpath_plugin,
             install_rpath: libs_rpath_plugin,
             install_dir: install_path_plugin,
             install: true
        )
