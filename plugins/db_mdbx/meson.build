plug_name = 'db_mdbx'

db_mdbx_src = files('''
    db_mdbx.c
'''.split())

lib_mdbx_src = files ('''
    libmdbx/mdbx.h
    libmdbx/src/mdbx.c
    libmdbx/src/osal.c
    libmdbx/src/version.c
'''.split())

if ((host_machine.system() == 'linux') or
    (host_machine.system() == 'darwin'))
    message ('Use lck-posix functionality in @0@ plugin on @1@ platform'.format(plug_name, host_machine.system()))
    lib_mdbx_src += 'libmdbx/src/lck-posix.c'
elif (host_machine.system() == 'windows')
    message ('Use lck-windows functionality in @0@ plugin on @1@ platform'.format(plug_name, host_machine.system()))
    lib_mdbx_src += 'libmdbx/src/lck-windows.c'
else
    error('This operating system not supported @0@'.format(host_machine.system()))
endif

db_mdbx_src += lib_mdbx_src

db_mdbx_dep = cc.find_library('pthread', required: true)

lib_db_mdbx = shared_library(plug_name,
                             db_mdbx_src,
                             dependencies: db_mdbx_dep,
                             include_directories: lib_core_inc)
