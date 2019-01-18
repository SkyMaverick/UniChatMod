#!/usr/bin/env python3

import os
from sys import argv

try:
    if os.path.exists(os.path.abspath (argv[1])) :
        for root, dirs, files in os.walk (os.path.abspath (argv[1])) :
            for f in files:
                print (os.path.abspath(f))
except:
    exit(1)

