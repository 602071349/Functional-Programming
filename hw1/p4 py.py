def sumlst (k,lst):
    if len(lst)<k:
        sum1=0
        for i in range(len(lst)):    
            sum1+=lst[i]
        return sum1    
    else:
        sum2=0
        for i in range(len(lst)):
            if i <k:
                sum2+=lst[i]
        return sum2

def addsum1 (lst,n,k):
    if n<=0:
        return lst
    else:
        x=sumlst(k,lst)
        return addsum1([x]+lst,n-1,k)
def addsum (n,k):
    return addsum1([1,1,0],n-2,k)
def Tec(n,k):
    if n==0:
        return addsum(0,k)[2]
    elif n==1:
        return addsum(0,k)[1]
    elif n==2:
        return addsum(0,k)[0]
        
    else:
        return addsum(n,k)[0]
    
        
