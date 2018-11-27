from utf8proc.make import job as u8p_build
from ucl.make import job as ucl_build

def dummy_job (path, *args):
    pass

depends = {
    'utf8proc'  : u8p_build,
    'ucl'       : ucl_build,
}

def build_dep (dep, target_path, *args):
    try:
        action = depends [dep]
    except KeyError as e:
        print ("I don't know this depends: {}".format(dep))
    else:
        action (target_path, args)
