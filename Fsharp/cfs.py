#!/usr/bin/python3

import sys
import os

def remove_extension(s): 
    for i in range(len(s)-1, 0, -1):
        if s[i] == '.':
            s = s[ : i]
            break
    return s 

fsharp_source = sys.argv[1]
executable = remove_extension(fsharp_source) + ".exe"

status = os.system("fsharpc " + fsharp_source)

if status == 0:
    os.system("chmod +x " + executable)
