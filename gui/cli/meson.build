cliapp_name = 'ucm_cli'
cliapp_src = files('''
    clui.c
'''.split()) + core_header_c

cliapp_dep = [
    cc.find_library('dl', required: true)
]

cli_app_cfg = configuration_data()

cli_app_cfg.set_quoted ('appname'          , meson.project_name())
cli_app_cfg.set_quoted ('version'          , meson.project_version())
cli_app_cfg.set_quoted ('path_modules'     , get_option ('path_modules_lookup'))

configure_file (input: 'cli_config.h.in',
                output: 'cli_config.h',
                configuration: cli_app_cfg)

if get_option('build_bundle')
    cliapp = executable (cliapp_name,
                 cliapp_src,
                 include_directories: lib_core_inc,
                 dependencies: cliapp_dep,
                 build_rpath: '$ORIGIN/.',
                 install_rpath: '$ORIGIN/.',
                 install: true,
                 install_dir: install_path_root
             )
else
    cliapp = executable (cliapp_name,
                 cliapp_src,
                 include_directories: lib_core_inc,
                 dependencies: cliapp_dep,
                 install: true,
                 install_dir: install_path_root
             )
endif
