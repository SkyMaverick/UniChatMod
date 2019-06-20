plugins = '''
    db_mdbx
    net_uc2
    log_file
'''.split()

plugins_only_nix = '''
'''.split()

plugins_only_win = '''
'''.split()

if target_machine.system() == 'windows'
    message ('Build windows-only plugins: @0@'.format(plugins_only_win))
    plugins += plugins_only_win
endif
if target_machine.system() == 'linux'
    message ('Build linux-only plugins: @0@'.format(plugins_only_nix))
    plugins += plugins_only_nix
endif

foreach plugin : plugins
    subdir (plugin)
endforeach
