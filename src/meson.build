app_name = 'ucm'

app_src = files('''
    app.c
    flags.c
    flags.h
    cmdline.c
    cmdline.h
    osal.h
    osal.c
'''.split()) + shared_headers

app_dep = [libucm_depends]

if get_option ('create_with_console_gui')
    app_src += files ('''
       curses/tui.c
       curses/tui.h
    '''.split())
 
    app_inc = include_directories ('.', './curses')

    if (osid == 'windows')
        app_dep_con += subproject('pdcurses').get_variable('pdcurses_dep')
        app_con = executable (''.join([app_name,'c']),
                     app_src,
                     dependencies   : app_dep + app_dep_con,
                     build_rpath    : libs_rpath,
                     install_rpath  : libs_rpath,
                     include_directories: app_inc,
                     install        : true,
                     install_dir    : install_path_app)
    else
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
endif

app = executable (app_name,
             app_src,
             dependencies   : app_dep,
             build_rpath    : libs_rpath,
             install_rpath  : libs_rpath,
             include_directories: app_inc,
             install        : true,
             install_dir    : install_path_app)
