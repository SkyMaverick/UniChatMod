p_matrix = {
    'uno'       : '''
         db_mdbx
         log_file
        '''.split(),
    'linux'     : '''
        '''.split(),
    'windows'   : '''
        '''.split(),
}

p_matrix_exp = {
    'uno'       : '''
         net_uc2
         gui_curses
        '''.split(),
    'linux'     : '''
        '''.split(),
    'windows'   : '''
        '''.split(),
}

plugins = []

foreach system, source : p_matrix
    if (system == 'uno') or (system == osid)
        plugins += source
    endif
endforeach

if get_option('create_with_experimental')
    warning ('Build with experimental plugins. This plugins maybe unstable.')
    foreach system, source : p_matrix_exp
        if (system == 'uno') or (system == osid)
            plugins += source
        endif
    endforeach
endif

foreach item : plugins
    message ('Build plugin: @0@'.format(item))
    subdir (item)
endforeach
