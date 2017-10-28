import random

def select(n,lst):
    if n<0 or n>len(lst):
        return "ERROR"
    return selecthelp(n,lst,[])


def selecthelp(n,lst,lst1):
    if n==0:
        return lst1
    else:
        
        x=random.randint(lst[0],lst[len(lst)-1])
        if check(x,lst1)==False:        
            return selecthelp(n-1,lst,[x]+lst1)
        return selecthelp(n,lst,lst1)

def check(x,lst1):
    if x in lst1:
        return True

    return False
    
    
