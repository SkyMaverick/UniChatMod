if get_option('create_with_console_gui')
    app_cflags += '''-DENABLE_CURSES_UI'''.split()

    app_dep_matrix = {
        'curses' : {
            'name'      : 'ncurses',
            'version'   : '>=6.0.0',
        },
        'panel' : {
            'name'      : 'panel',
            'version'   : '>=6.0.0',
        }
    }
    foreach lib, opts : app_dep_matrix
        curses_dep = dependency (opts.get('name'), version: opts.get('version'), required: false,
                not_found_message: 'Don\'t found dependency. Try @0@ for found library, but not guaranteed it correct version'.format(ccid))
        if not curses_dep.found()
            curses_dep = cc.find_library (lib, required: true)
        endif
        app_dep += curses_dep
    endforeach
endif

app = executable (app_name,
             app_src,
             c_args         : app_cflags,
             dependencies   : app_dep,
             build_rpath    : libs_rpath,
             install_rpath  : libs_rpath,
             include_directories: app_inc,
             link_args      : ['-lpanel', '-lcurses'],
             install        : true,
             install_dir    : install_path_app)
