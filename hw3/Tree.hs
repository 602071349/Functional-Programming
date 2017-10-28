--Jiazhou Liu ljzhou@bu.edu
module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)


	
mapT f Leaf = Leaf
mapT f (Node a l r)= Node (f a) (mapT f l) (mapT f r)

foldT f b Leaf = b
foldT f b (Node a l r)= f a (foldT f b l) (foldT f b r)

add3  m n p =n+p
leafCount Leaf= 1
leafCount (Node a l r)= foldT add3 1 (Node a l r)

add2 m n p = n+p+1

nodeCount Leaf= 0
nodeCount (Node a l r)=foldT add2 0 (Node a l r)

add1 m n p = n + 1
add0 m n p = p + 1

height Leaf=0
height (Node a l r)=max (foldT add1 0 (Node a l r)) (foldT add0 0 (Node a l r))

perfect Leaf= True
perfect (Node a l r)
	|(height l /= height r)=False
	|otherwise = (perfect l)&&(perfect r)
	
degenerate Leaf=True
degenerate (Node a l r)
	|(l/=Leaf)&&(r/=Leaf) = False
	|otherwise = (degenerate l)&&(degenerate r)

add5 a b c = [a]++b++c
	
list Leaf = []
list (Node a l r)
	|(degenerate (Node a l r)==False) = []
	|otherwise = foldT add5 [] (Node a l r)
