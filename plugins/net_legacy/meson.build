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

nl_dep = [
    cc.find_library('pthread', required: true),
    cc.find_library('ucl', required: true,
                    dirs: meson.build_root())
]

nl_mdbx = shared_library(plug_name,
                             nl_src,
                             dependencies: nl_dep,
                             include_directories: lib_core_inc,
                             install: true,
                             install_dir: install_path_mods)
