plug_name = 'net_unichat'

netuc_src = files('''
    network.c
    network.h
    protocol.c
    protocol.h
    netuc.h
    netuc.c
    rc6/rc6.c
    rc6/rc6.h
'''.split()) + shared_headers

netuc_dep = [
    system_dependencies
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
netuc_dep += ucl_dep

netuc_lib = shared_library(plug_name,
                           netuc_src,
                           dependencies: netuc_dep,
                           include_directories: libucm_inc,
                           install: true,
                           install_dir: install_path_mods)
