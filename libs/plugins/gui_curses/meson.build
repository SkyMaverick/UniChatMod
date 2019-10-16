tuilib_name = 'ui_ucmc'
tuilib_src = files('''
    tui.c
    tui.h
'''.split()) + shared_headers

tuilib_dep = [
    system_dependencies
]

if (osid == 'windows')
    if not get_option('custom_libs')
        pdc_dep = cc.find_library('pdcurses', required: false)
        if not pdc_dep.found()
            pdcdep_lib = subproject('pdcurses')
            pdc_dep = pdcdep_lib.get_variable('pdcurses_dep')
        endif
    else
        pdcdep_lib = subproject('pdcurses')
        pdc_dep = pdcdep_lib.get_variable('pdcurses_dep')
    endif

    tuilib_dep += pdc_dep
else
    tuilib_dep += cc.find_library ('ncurses', required: true)
endif

tuilib = shared_library (tuilib_name,
             tuilib_src,
             include_directories: libs_inc,
             build_rpath: libs_rpath_plugin,
             install_rpath: libs_rpath_plugin,
             dependencies: tuilib_dep,
             install_dir: install_path_plugin,
             install: true
         )
