lazapp_proj = 'UniChatMod.lpi'

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

lazbuild_ars = [
    '--build-all'
]

if target_machine.system() == 'windows'
    if target_machine.cpu_family() == 'x86'
        lazbuild_ars += '--operating-system=win32 --ws=win32'
    elif target_machine.cpu_family() == 'x86_64'
        lazbuild_ars += '--operating-system=win64 --ws=win64'
    endif
else
    lazbuild_ars += '--operating-system=linux --ws=gtk2'
endif

if target_machine.cpu_family() == 'x86'
    lazbuild_args += '--cpu=i386'
else
    lazbuild_ars  += '--cpu=x86_64'
endif

lazapp = run_command (lazbuild, lazbuild_ars)

message(lazapp.stdout())    

if lazapp.returncode() != 0
    error('Stop compilation code: @0@. @1@'.format(lazapp.returncode(), lazapp.stdout()))
endif
