#python is easir for me, as I am familiar with python
#haskell has a benefit as it addopts lazy evaluation,while we need generater in python
#both languages are flexiable, but python is easier to debug
import sys
input1=int(sys.argv[1])
input2=int(sys.argv[2])
def prefix(n,xs):
    if xs==[] or n==0:
        return []
    else:
        return [xs[0]] + prefix(n-1,xs[1:])

def suffix(n,xs):
    if xs==[] or n==0:
        return xs
    else:
        return suffix(n-1,xs[1:])

def split(n,y,xs):
    if suffix(n,xs)==[]:
        return xs
    else:
        return prefix(n,xs)+[y]+split(n,y,suffix(n,xs))
    
def plane(r):
    return [((x/r)*1.0,(y/r)*1.0) for y in range(-r,r+1) for x in range(-r*2,r+1)]

def disp(d,lst):
    for i in lst:
        if i[0]>d:
            return i[1]
    return ' '

def p1(u,v,x,y):
    return(u*u-v*v+x,2*u*v+y)
        
def orbit(xy):
    u=0
    v=0
    l1=(0,0)
    while True:
            l1=p1(u,v,xy[0],xy[1])
            yield l1
            k=u
            s=v
            u=k*k-s*s+xy[0]
            v=2*k*s+xy[1]

def get(x,xy):
    result = 0
    a = orbit(xy)
    for i in range(x):
        result = next(a)
    return result
def norm(xy):
    return xy[0]*xy[0]+xy[1]*xy[1]
        
    
    
def mandelbrot(r,i,l):
    return split((r*3+1),'\n',[disp(c,l) for c in [norm(b) for b in[get(i,a) for a in plane(r)]]])

a = mandelbrot(input1,input2,[(0.15,'#'),(0.5,'x'),(1,'.')])
b = ''.join(a)
print(b)
    


                                

