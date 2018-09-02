plug_name = 'net_legacy'

net_legacy_src = files('''
    network.c
    network.h
    net_legacy.h
    net_legacy.c
'''.split()) + core_header_c

net_legacy_dep = cc.find_library('pthread', required: true)

net_legacy_mdbx = shared_library(plug_name,
                             net_legacy_src,
                             dependencies: net_legacy_dep,
                             include_directories: lib_core_inc,
                             install: true,
                             install_dir: install_path_mods)
