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
         gui_sdl2
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
    if get_option ('create_with_gui') == false
        if item.startwith('gui')
            continue
        endif
    endif

    message ('Build plugin: @0@'.format(item))
    subdir (item)
endforeach
