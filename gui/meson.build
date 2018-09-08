if get_option('gui_enable_lazarus')
    message ('Build GUI Lazarus application on @0@ @1@ target platform'.format(target_machine.system(), target_machine.cpu()))
    subdir('lazarus')
endif

if get_option('gui_enable_cli')
    message ('Build console user interface for target platform: @0@'.format(target_machine.system()))
    subdir('cli')
endif
