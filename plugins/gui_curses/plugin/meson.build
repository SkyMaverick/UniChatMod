tuilib_name = 'ui_ucmc'
tuilib_src = files('''
    tui.c
    tui.h
'''.split()) + shared_headers

tuilib_dep = [
    system_dependencies
]

if not get_option('custom_libs')
    pdc_dep = cc.find_library('pdcurses', required: false)
    if not pdc_dep.found()
        pdcdep_lib = subproject('pdcurses', default_options: ['='.join (['path_install', install_path_libs])])
        pdc_dep = pdcdep_lib.get_variable('pdcurses_dep')
    endif
else
    pdcdep_lib = subproject('pdcurses', default_options: ['='.join (['path_install', install_path_libs])])
    pdc_dep = pdcdep_lib.get_variable('pdcurses_dep')
endif

tuilib_dep += pdc_dep

tuilib = shared_library (tuilib_name,
             tuilib_src,
             include_directories: libs_inc,
             build_rpath: libs_rpath,
             install_rpath: libs_rpath,
             dependencies: tuilib_dep,
             install: true,
             install_dir: install_path_mods
         )
