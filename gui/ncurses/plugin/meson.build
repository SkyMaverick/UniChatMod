tuilib_name = 'ui_ucmc'
tuilib_src = files('''
    tui.c
    tui.h
'''.split()) + shared_headers

tuilib_dep = [
    cc.find_library ('dl'       , required: true),
    cc.find_library ('menu'     , required: true),
    cc.find_library ('panel'    , required: true),
    cc.find_library ('form'     , required: true),
    cc.find_library ('ncurses'  , required: true),
    system_dependencies
]

tuilib = shared_library (tuilib_name,
             tuilib_src,
             include_directories: libucm_inc,
             dependencies: tuilib_dep,
             install: true,
             install_dir: install_path_mods
         )
