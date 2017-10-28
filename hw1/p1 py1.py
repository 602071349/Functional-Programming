
def fact(n):
    return prod(down (n))

def prod(n):
    if n==[]:
        return 1
    else:
        return n[0]*prod(n[1:])
def down(n):
    if n==0:
        return []
    else:
        return [n]+down(n-1)

