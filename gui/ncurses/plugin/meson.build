tuilib_name = 'ui_ucmc'
tuilib_src = files('''
    tui.c
    tui.h
'''.split()) + core_header_c

tuilib_dep = [
    cc.find_library ('dl'       , required: true),
    cc.find_library ('menu'     , required: true),
    cc.find_library ('panel'    , required: true),
    cc.find_library ('form'     , required: true),
    cc.find_library ('ncurses' , required: true)
]

tuilib = shared_library (tuilib_name,
             tuilib_src,
             include_directories: lib_core_inc,
             dependencies: tuilib_dep,
             install: true,
             install_dir: install_path_mods
         )
