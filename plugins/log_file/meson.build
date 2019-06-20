plug_name = 'log_file'

stdlog_src = files('''
    log_file.c
'''.split()) + shared_headers

stdlog_dep = [
    system_dependencies
]

lib_stdlog = shared_library(plug_name,
                             stdlog_src,
                             dependencies: stdlog_dep,
                             include_directories: libucm_inc,
                             build_rpath: libs_rpath,
                             install_rpath: libs_rpath,
                             install: true,
                             install_dir: install_path_mods)
