plug_name = 'db_mdbx'

db_mdbx_src = files('''
    db_mdbx.h
    db_mdbx.c
    db_mdbx_api.c
    db_mdbx_api.h
    db_mdbx_base.h
    db_mdbx_base.c
'''.split()) + core_header_c

if ((host_machine.system() == 'linux') or
    (host_machine.system() == 'darwin'))
    message ('Use lck-posix functionality in @0@ plugin on @1@ platform'.format(plug_name, host_machine.system()))
elif (host_machine.system() == 'windows')
    message ('Use lck-windows functionality in @0@ plugin on @1@ platform'.format(plug_name, host_machine.system()))
else
    error('This operating system not supported @0@'.format(host_machine.system()))
endif

db_mdbx_dep = [
    cc.find_library('pthread', required: true),
    cc.find_library('mdbx', required: true,
                    dirs: paths_custom_libs),
]

lib_db_mdbx = shared_library(plug_name,
                             db_mdbx_src,
                             dependencies: db_mdbx_dep,
                             include_directories: lib_core_inc,
                             install: true,
                             install_dir: install_path_mods)
