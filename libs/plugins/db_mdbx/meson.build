plug_name = 'db_mdbx'

db_mdbx_src = files('''
    db_mdbx.h
    db_mdbx.c
    mdbx_core.c
    mdbx_core.h
'''.split()) + shared_headers

db_mdbx_dep = [
    system_dependencies,
    libucm_depends
]

mdbx_dep = disabler()
if not get_option('custom_libs')
    mdbx_dep = dependency ('libmdbx', version: '>=0.31', required: false)
    if not mdbx_dep.found()
        mdbx_dep = cc.find_library('mdbx', required: false)
    endif
endif
if not mdbx_dep.found()
    mdbx_dep = subproject('mdbx').get_variable('libmdbx')
endif
db_mdbx_dep += mdbx_dep

lib_db_mdbx = shared_library(plug_name,
                             db_mdbx_src,
                             dependencies: db_mdbx_dep,
                             build_rpath: libs_rpath_plugin,
                             install_rpath: libs_rpath_plugin,
                             install_dir: install_path_plugin,
                             install: true )
