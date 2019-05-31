
if meson.is_subproject()
    pdc_lib_shared = shared_library (meson.project_name(),
                                    pdc_source,
                                    dependencies: pdc_dep,
#                                    include_directories: pdc_includes,
                                    install: true,
                                    install_dir: get_option('path_install'))
    pdcurses_dep = declare_dependency (
#                   include_directories: pdc_includes,
                    link_with: pdc_lib_shared
                    )
else
    pdc_lib_shared = shared_library (meson.project_name(),
                                    pdc_source,
                                    dependencies: pdc_dep,
#                                    include_directories: pdc_includes,
                                    install: true)
endif
