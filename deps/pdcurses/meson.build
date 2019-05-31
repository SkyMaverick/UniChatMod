project ('pdcurses', 'c',
         version: '3.8',
         license: 'Public domain',
         meson_version: '>=0.49')

cc = meson.get_compiler ('c')

ccid = cc.get_id()
sysid = target_machine.system()

pdc_cflags       = []
pdc_defines      = []
pdc_deps         = []

# Include core files list
subdir ('pdcurses')

pdc_includes = include_directories('.')

pdc_backend = ''
if ccid == 'windows'
    pdc_backend = 'wincon'
else
    pdc_backend = 'x11'
endif

message ('Select PDCurses backend: @0@'.format(pdc_backend))
subdir(pdc_backend)
