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

app_src_con = files ('''
   curses/tui.c
   curses/tui.h
'''.split())

app_inc = include_directories ('.', './curses')
app_dep = [libucm_depends]
if not (osid == 'windows')
    app_dep += cc.find_library ('dl', required: true)
endif
app_cflags = []
app_defs = []

if osid == 'windows'
# create console application
    if get_option('create_with_console_gui')
        if ccid == 'msvc'
            app_cflags += '''
                /DENABLE_CURSES_UI
                /DWIN_CONSOLE_APP
            '''.split()
        else
            app_cflags += '''
                -DENABLE_CURSES_UI
                -DWIN_CONSOLE_APP
            '''.split()
        endif

        app_dep_con = subproject('pdcurses').get_variable('pdcurses_dep')
        app_con = executable (''.join([app_name,'c']),
                     app_src + app_src_con,
                     dependencies   : app_dep + [app_dep_con],
                     c_args         : app_cflags,
                     build_rpath    : libs_rpath,
                     install_rpath  : libs_rpath,
                     include_directories: app_inc,
                     install        : true,
                     install_dir    : install_path_app)
    endif
else
    if get_option('create_with_console_gui')
        app_cflags += '''-DENABLE_CURSES_UI'''.split()
        app_src += app_src_con

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
endif
