#4**n
def recursive (n):
    if n <= 1:
        return n
    elif n==2:
        return 1
    elif n==3:
        return 2
    else:
        return recursive (n-1) + recursive(n-2) + recursive(n-3) + recursive (n-4) 
