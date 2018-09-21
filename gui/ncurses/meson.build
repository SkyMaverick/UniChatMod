tuiapp_name = 'ucmc'
tuiapp_src = files('''
    tui.c
    tui.h
    main.c
    main.h
'''.split()) + core_header_c

tuiapp_dep = [
    cc.find_library ('dl'       , required: true),
    cc.find_library ('menu'     , required: true),
    cc.find_library ('panel'    , required: true),
    cc.find_library ('form'     , required: true),
    cc.find_library ('ncurses'  , required: true),
]

tui_app_cfg = configuration_data()

tui_app_cfg.set_quoted ('appname'          , meson.project_name())
tui_app_cfg.set_quoted ('version'          , meson.project_version())
tui_app_cfg.set_quoted ('path_modules'     , get_option ('path_modules_lookup'))

configure_file (input: 'tui_config.h.in',
                output: 'tui_config.h',
                configuration: tui_app_cfg)

if get_option('build_bundle')
    tuiapp = executable (tuiapp_name,
                 tuiapp_src,
                 include_directories: lib_core_inc,
                 dependencies: tuiapp_dep,
                 build_rpath: '$ORIGIN/.',
                 install_rpath: '$ORIGIN/.',
                 install: true,
                 install_dir: install_path_root
             )
else
    tuiapp = executable (tuiapp_name,
                 tuiapp_src,
                 include_directories: lib_core_inc,
                 dependencies: tuiapp_dep,
                 install: true,
                 install_dir: install_path_root
             )
endif
