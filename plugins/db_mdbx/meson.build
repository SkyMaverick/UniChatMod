plug_name = 'db_mdbx'

db_mdbx_src = files('''
    db_mdbx.h
    db_mdbx.c
    mdbx_core.c
    mdbx_core.h
'''.split()) + shared_headers

db_mdbx_dep = [
    system_dependencies
]

if not get_option('custom_libs')
    mdbx_dep = cc.find_library ('mdbx', required: false)
    if not mdbx_dep.found()
        cmake = import ('cmake')
        mdbx_proj = cmake.subproject('mdbx')
        mdbx_dep = mdbx_proj.dependency ('mdbx')
    endif
else
        cmake = import ('cmake')
        mdbx_proj = cmake.subproject('mdbx',
                    cmake_options: [
                        '='.join (['-DPROVIDE_VERSIONINFO','OFF'])
                    ])
        mdbx_dep = mdbx_proj.dependency ('mdbx')
endif
db_mdbx_dep += mdbx_dep

lib_db_mdbx = shared_library(plug_name,
                             db_mdbx_src,
                             dependencies: db_mdbx_dep,
                             include_directories: libs_inc,
                             build_rpath: libs_rpath_plugin,
                             install_rpath: libs_rpath_plugin,
                             install_dir: install_path_plugin,
                             install: true
                             )
