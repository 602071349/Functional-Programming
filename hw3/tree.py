#Jiazhou Liu ljzhou@bu.edu

class Tree:
    def __init__(self):
        self.value=None
        self.left=None
        self.right=None
        
    def __eq__(self,other):
        if (self.value==None)&(other.value==None):
            return True
        elif(self.value==None)&(other.value!=None):
            return False
        elif(self.value!=None)&(other.value==None):
            return False
        else:
            return (self.value==other.value)&(self.left==other.left)&(self.right==other.right)
    
    def leafcount(self):
        if (self.value==None):
            return 1
        else:
            return self.left.leafcount()+self.right.leafcount()
            
    def nodecount(self):
        if (self.value==None):
            return 0
        else:
            return self.left.nodecount()+self.right.nodecount()+1
    def height(self):
        if (self.value==None):
            return 0
            
        else:
            return max(self.left.height()+1,self.right.height()+1)
    def perfect(self):
        if (self.value==None):
            return True
        elif(self.left.height()!=self.right.height()):
             return False
        else:
            return self.left.perfect()and self.right.perfect()

    def degenerate(self):
        if (self.value==None):
            return True
        elif(self.left.value!=None)and(self.right.value!=None):
            return False
        else:
            return self.left.degenerate()and self.right.degenerate()

    def list(self):
        if (self.value==None):
            return []
        elif(self.degenerate()==False):
            return []
        else:
            return [self.value]+self.left.list()+self.right.list()
            
        
def Node(value,left,right):
    n=Tree()
    n.value=value
    n.left=left
    n.right=right
    return n
def Leaf():
    n=Tree()
    return n    
    
