lazapp_proj = 'UniChatMod.lpr'
lazbuild_script = 'build.sh'

# -- found and check lazbuild ---
lazbuild = find_program('lazbuild', required: true)
lazbuild_support = '>=1.8.0'

if lazbuild.found()
    lazbuild_version = run_command(lazbuild, '--version').stdout().strip()
    if (lazbuild_version.version_compare(lazbuild_support))
        message ('Lazarus build app version: @0@'.format(lazbuild_version))
    else
        warning ('Lazarus build app version @0@ not tested. Use @1@ version'.format(lazbuild_version, lazbuild_support))
    endif
else
    warning ('Lazarus build app NOT found')
endif

# -- found and check shell ---
shell = find_program('sh', required: true)
if shell.found()

endif


# -- define OS-specified arguments ---

shell_run = configuration_data()

shell_run.set('sh_project', lazapp_proj)

if target_machine.system() == 'windows'
    if target_machine.cpu_family() == 'x86'
        shell_run.set('sh_os', 'win32')
        shell_run.set('sh_ws', 'win32')
    elif target_machine.cpu_family() == 'x86_64'
        shell_run.set('sh_os', 'win64')
        shell_run.set('sh_ws', 'win64')
    endif
else
        shell_run.set('sh_os', 'linux')
        shell_run.set('sh_ws', 'gtk2')
endif
if target_machine.cpu_family() == 'x86'
        shell_run.set('sh_arch', 'i386')
else
        shell_run.set('sh_arch', 'x86_64')
endif

configure_file (input: 'build.sh.in',
                output: lazbuild_script,
                configuration: shell_run)

lazbuild_script = join_paths(meson.current_build_dir(), lazbuild_script)

lazapp = run_target ('lazgui',
                     depends: [libcore, libcore_a],
                     command: [lazbuild_script])

subdir('icons')
