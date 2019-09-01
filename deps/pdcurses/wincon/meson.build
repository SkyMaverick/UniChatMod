pdc_cflags = []
pdc_lflags = []

if get_option('buildtype').endswith('debug')
    pdc_cflags += '''
        /Z7
        /DPDCDEBUG
    '''.split()
    pdc_lflags += '''
        /debug
        /pdb:none
    '''.split()
endif

foreach flag : pdc_cflags
    if cc.has_argument(flag)
        add_project_arguments(flag, language: 'c')
    endif
endforeach
foreach flag : pdc_lflags
    if cc.has_link_argument(flag)
        add_project_link_arguments(flag, language: 'c')
    endif
endforeach

if get_option ('enable_widec')
    add_project_arguments('/DPDC_WIDE', language: 'c')
endif
if get_option ('enable_force_utf8')
    add_project_arguments('/DPDC_FORCE_UTF8', language: 'c')
endif
if get_option ('no_infoex')
    add_project_arguments('/DHAVE_NO_INFOEX', language: 'c')
endif

pdc_deps += [
    cc.find_library ('user32', required: true),
    cc.find_library ('advapi32', required: true)
]

pdc_source = files ('''
        pdcwin.h
        pdcclip.c
        pdcdisp.c
        pdcgetsc.c
        pdckbd.c
        pdcscrn.c
        pdcsetsc.c
        pdcutil.c
'''.split()) + pdc_core

add_project_arguments ('/DPDC_DLL_BUILD', language: 'c')
if meson.is_subproject()
    pdc_lib_shared = shared_library (meson.project_name(),
                                    pdc_source,
                                    include_directories: pdc_includes,
                                    dependencies: pdc_deps,
                                    install: true,
                                    install_dir: get_option('path_install'))
    pdcurses_dep = declare_dependency (
                    include_directories: pdc_includes,
                    link_with: pdc_lib_shared
                    )
else
    pdc_lib_shared = shared_library (meson.project_name(),
                                    pdc_source,
                                    include_directories: pdc_includes,
                                    dependencies: pdc_deps,
                                    install: true)
endif
