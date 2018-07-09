import os
from .convert import conv_p2p

resolutions = [16, 32, 48, 64, 128, 256]
src_ext = '.svg'
out_ext = '.png'

def svg2png (in_path, out_path) :
    src_path = os.path.realpath (os.path.normpath (in_path))
    out_path = os.path.realpath (os.path.normpath (out_path))

    for res in resolutions :
        conv_p2p (src_path, out_path, src_ext, out_ext, res)
