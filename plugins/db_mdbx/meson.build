plug_name = 'db_mdbx'

db_mdbx_src = files('''
    db_mdbx.h
    db_mdbx.c
    db_mdbx_api.c
    db_mdbx_api.h
    db_mdbx_base.h
    db_mdbx_base.c
'''.split()) + core_header_c

db_mdbx_dep = [
]

if not get_option('custom_libs')
    mdbx_dep = cc.find_library ('mdbx', required: false)
    if not mdbx_dep.found()
        mdbx_lib = subproject('mdbx', default_options: [ '='.join ( ['path_install', install_path_libs] )] )
        mdbx_dep = mdbx_lib.get_variable('libmdbx_dep')
    endif
else
    mdbx_lib = subproject('mdbx', default_options: [ '='.join ( ['path_install', install_path_libs] )] )
    mdbx_dep = mdbx_lib.get_variable('libmdbx_dep')
endif
db_mdbx_dep += mdbx_dep

lib_db_mdbx = shared_library(plug_name,
                             db_mdbx_src,
                             dependencies: db_mdbx_dep,
                             include_directories: lib_core_inc,
                             build_rpath: libs_rpath,
                             install_rpath: libs_rpath,
                             install: true,
                             install_dir: install_path_mods)
