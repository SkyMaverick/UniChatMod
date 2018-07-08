lazapp_proj = 'UniChatMod.lpr'
lazbuild_script = 'lazui.py'
icons_src = join_paths (meson.current_source_dir(), 'icons', 'svg')
icons_out = join_paths (meson.current_source_dir(), 'icons', 'out')

# -- found and check lazbuild ---
app_lazbuild = find_program('lazbuild', required: true)
app_lazbuild_support = '>=1.8.0'

if app_lazbuild.found()
    app_lazbuild_version = run_command(app_lazbuild, '--version').stdout().strip()
    if (app_lazbuild_version.version_compare (app_lazbuild_support))
        message ('Lazarus build app version: @0@'.format(app_lazbuild_version))
    else
        warning ('Lazarus build app version @0@ not tested. Use @1@ version'.format(app_lazbuild_version, app_lazbuild_support))
    endif
else
    error ('Lazarus build app NOT found')
endif

# -- found and check ImageMagick ---
app_convert = find_program('convert', required: true)
if app_convert.found() 
    app_convert_ver = run_command (app_convert, '-version').stdout().strip().split('\n')[0]
    message ('Convert application version: @0@'.format(app_convert_ver))
else
    error ('Convert application (ImageMagic part) NOT found')
endif

# -- found and check Python ---
app_py = import('python3').find_python()
if app_py.found()
    app_py_ver = run_command (app_py, '--version').stdout().strip()
    message ('Python version found: @0@'.format(app_py_ver))
else
    error ('Python NOT found')
endif

py_data = [app_lazbuild.path()]
py_data += [ join_paths (meson.current_source_dir(), lazapp_proj) ]

if target_machine.cpu_family() == 'x86'
        py_data += ['i386']
else
        py_data += ['x86_64']
endif
if target_machine.system() == 'windows'
    if target_machine.cpu_family() == 'x86'
        py_data += ['win32', 'win32']
    elif target_machine.cpu_family() == 'x86_64'
        py_data += ['win64', 'win64']
    endif
else
        py_data += ['linux','gtk2']
endif

py_data += [icons_src, icons_out]
py_cmd = [app_py, '-c', 'from lazui import lazui_build; lazui_build("@0@", "@1@", "@2@", "@3@", "@4@", "@5@", "@6@")'.format (
                        py_data[0], py_data[1], py_data[2], py_data[3], py_data[4], py_data[5], py_data[6] )
]

lazapp = run_target ('lazgui',
                     depends: [libcore, libcore_a],
                     command: [py_cmd])
