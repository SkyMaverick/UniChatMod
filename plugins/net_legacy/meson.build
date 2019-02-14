plug_name = 'net_legacy'

nl_src = files('''
    network.c
    network.h
    protocol.c
    protocol.h
    net_legacy.h
    net_legacy.c
    rc6/rc6.c
    rc6/rc6.h
'''.split()) + shared_headers

nl_dep = [
]

if not get_option('custom_libs')
    ucl_dep = cc.find_library ('ucl', required: false)
    if not ucl_dep.found()
        ucl_lib = subproject('ucl', default_options: [ '='.join ( ['path_install', install_path_libs] )] )
        ucl_dep = ucl_lib.get_variable('ucl_dep')
    endif
else
    ucl_lib = subproject('ucl', default_options: [ '='.join ( ['path_install', install_path_libs] )] )
    ucl_dep = ucl_lib.get_variable('ucl_dep')
endif
nl_dep += ucl_dep

nl_mdbx = shared_library(plug_name,
                             nl_src,
                             dependencies: nl_dep,
                             include_directories: lib_core_inc,
                             install: true,
                             install_dir: install_path_mods)
