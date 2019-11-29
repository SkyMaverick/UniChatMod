app_name = 'ucm'

app_src = files('''
    alloy.c
'''.split()) + shared_headers

subdir('common')

app_inc = include_directories ('.', './common')
app_dep = [libucm_depends]
app_cflags = []
app_defs = []

if osid == 'windows'
    subdir('win')
else
    subdir('posix')
endif
