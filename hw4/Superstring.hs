--Jiazhou Liu ljzhou@bu.edu
--Among the four,bestPair performs best, just as well as the optimal superstring;other greedy algorithms also perfrom ok
--not as well as bestPair.

module Superstring where
import Prelude hiding (compare)

import Data.List

type Str a = [a]

overlap:: Eq a => (Str a, Str a) -> Int
overlap (a,b) 
	|(a==[])=0
	|(isPrefixOf a b) = length a
	|otherwise = overlap ((tail a),b)

contains:: Eq a => Str a -> Str a -> Bool
contains a b = isInfixOf b a

o ::Eq a =>Str a->Str a->Str a
a `o` b = a ++ (drop (overlap (a,b)) b)

naive::Eq a=>[Str a]->Str a
naive a = foldr (o) [] a
	
maximize:: Eq a =>(a->Int)->a->a->a
maximize f a b
	|((f a)>(f b))=a
	|otherwise=b
	
minimize:: Eq a =>(a->Int)->a->a->a
minimize f a b
	|((f a)<(f b))=a
	|otherwise=b
	
update:: Eq a =>[Str a]->(Str a,Str a)->[Str a]
update x (a,b)=(a`o`b):(filter (not.contains (a`o`b)) x)


helper list []=list
helper list (x:xs)
	|(x `elem` list)=helper list xs
	|otherwise=helper (list ++ [x]) xs
	
allPairs::Eq a => [Str a] ->[(Str a,Str a)]
allPairs a =helper [] ([(x,y)|x<-a,y<-a,x/=y])

superstring::Eq a=> ([Str a]->[(Str a,Str a)])->[Str a]->Str a
superstring f [] =[]
superstring f [a]=a
superstring f list= foldr (minimize (length)) (naive list) [superstring f x|x<-[update list y| y <-(f list)]]

optimal::Eq a =>[Str a]->Str a
optimal a =superstring allPairs a

firstPair:: Eq a =>[Str a]->[(Str a,Str a)]
firstPair a =[((a!!0),(a!!1))]

helper1 a=[((a!!0),x)|x<-a,x/=(a!!0) ]

helper3 [] value pair= [pair]
helper3 (x:xs) value pair
	|((overlap x)>value)=helper3 xs (overlap x) x
	|otherwise=helper3 xs value pair

bestWithFirst ::Eq a => [Str a] -> [(Str a,Str a)]
bestWithFirst a =helper3 (helper1 a) 0 ((helper1 a)!!0)

bestPair ::Eq a =>[Str a]->[(Str a,Str a)]
bestPair a =helper3 (allPairs a) 0 ((allPairs a)!!0)

helper4 a=[((last a),x)|x<-a,x/=(last a)]
bestWithEnd a=helper3 (helper4 a) 0 (last (helper4 a))

greedy:: Eq a =>[Str a]->Str a
greedy a =superstring bestWithEnd a

compare:: Eq a =>([Str a] ->Str a)->([Str a]->Str a)->[Str a]->Double
compare f1 f2 a=( (fromIntegral (length (f1 a)))/(fromIntegral (length (f2 a))))






	


















	
