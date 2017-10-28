def addsum1 (lst,n):
    if n<=0:
        return lst
    else:
        x=lst[0]+lst[1]+lst[2]+lst[3]
        return addsum1([x]+lst,n-1)

def addsum (n):
    return addsum1([2,1,1,0],n-3)

def Tec(n):
    if n==0:
        return addsum(0)[3]
    elif n==1:
        return addsum(1)[2]
    elif n==2:
        return addsum(2)[1]
    elif n==3:
        return addsum(3)[0]
    else:
        return addsum(n)[0]
    
       
    
