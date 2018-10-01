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
'''.split()) + core_header_c

# subdir ('ucl')

nl_dep = cc.find_library('pthread', required: true)

if get_option('build_with_syslibs')
    nl_ucl_dep = cc.find_library('ucl', required: false)
else
    nl_ucl_dep = dependency('ucl', fallback:['ucl','ucl_dep'])
endif

nl_mdbx = shared_library(plug_name,
                             nl_src,
                             dependencies: [nl_dep, nl_ucl_dep],
                             include_directories: lib_core_inc,
                             install: true,
                             install_dir: install_path_mods)
