def info (arg):
    print ('{blue}{txt}{endcolor}'.format (blue='\033[36;1m', txt=arg, endcolor='\033[0m'))

def error (arg):
    print ('{red}{txt}{endcolor}'.format (red='\033[31;1m', txt=arg, endcolor='\033[0m'))

def hint (arg):
    print ('{yellow}{txt}{endcolor}'.format (yellow='\033[33;1m', txt=arg, endcolor='\033[0m'))