import os
import sys
import subprocess

def valid_res( vres ) :
    # TODO max / min size validate if it's need
    return True

def convert( fain, faout, cres ):
    params = ['convert','-density', '100', '-background', 'None', fain, '-support', '0.1', '-resize', str(cres), faout]
    subprocess.check_call( params )

def conv_p2p (src_rpath, out_rpath, src_ext, out_ext, res) :
    if not valid_res( res ) :
        print ("This resolution is missing")
        return False
    if not os.path.exists( src_rpath ) :
        print ("This path not found: %s" % ( src_rpath ))
        return False
    
    res = str(res)
    out_rpath = os.path.join (out_rpath, 'x'.join([res, res]))
    for src_dir, dirs, files  in os.walk( src_rpath ):
        dst_dir = src_dir.replace( src_rpath, out_rpath, 1 )
        if not os.path.exists( dst_dir ) :
            os.makedirs( dst_dir )
        for sfname in files :
            if sfname.endswith( src_ext ) :
                dfname  = sfname.replace( src_ext, out_ext, 1 )
                fsource = os.path.join( src_dir, sfname )
                ftarget = os.path.join( dst_dir, dfname )
                print ("Convert to %sx%s : %s\t>>\t%s" % (res, res, sfname, dfname))
                convert ( fsource, ftarget, res )
    return True
