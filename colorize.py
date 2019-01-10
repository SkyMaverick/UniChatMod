import platform

def info (arg):
    if platform.system().lower() == 'linux':
        print ('{blue}{txt}{endcolor}'.format (blue='\033[36;1m', txt=arg, endcolor='\033[0m'))
    else:
        print (arg)

def error (arg):
    if platform.system().lower() == 'linux':
        print ('{red}{txt}{endcolor}'.format (red='\033[31;1m', txt=arg, endcolor='\033[0m'))
    else:
        print (arg)

def hint (arg):
    if platform.system().lower() == 'linux':
        print ('{yellow}{txt}{endcolor}'.format (yellow='\033[33;1m', txt=arg, endcolor='\033[0m'))
    else:
        print (arg)
