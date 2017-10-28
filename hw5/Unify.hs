--Jiazhou Liu ljzhou@bu.edu
module Unify
(get,
emp,
sub,
Substitutable(..),
Unifiable(..),
Subst(..),
unify,
subst,
unresolved,
resolve,
cmb)
	where

data Subst a =S [(String,a)] deriving Show

emp::Subst a
emp  = (S [])

sub:: String -> a -> Subst a
sub string a = S [(string,a)]

get::String -> Subst a -> Maybe a
get string (S [])= Nothing
get string (S (x:xs)) 
	|((fst x)==string)= Just (snd x)
	|otherwise=get string (S xs)
	
class Substitutable a where
	subst::Subst a -> a -> a
	
class (Eq a, Substitutable a) => Unifiable a where
	unify::a -> a -> Maybe (Subst a)
	
unresolved:: (Eq a) => [(String,a)] -> Maybe (String,a,a)
unresolved lt
	|((helper2 lt [])==[])= Nothing
	|otherwise=Just ((fst ((helper2 lt [])!!0)),(snd ((helper2 lt [])!!0)),(snd ((helper2 lt [])!!1))) 
	
helper (str,a) []=[]
helper (str,a) (x:xs)
	|((fst x)==str)=[(str,a),x]
	|otherwise=helper (str,a) xs

helper2 [] lst =lst
helper2 (x:xs) lst 
	|(lst==[])=helper2 xs (helper x xs)
	|otherwise=helper2 xs lst
	
resolve::Unifiable a => Subst a -> Maybe (Subst a)
resolve (S a) 
	= case (unresolved a) of
							Nothing -> Just (S a)
							Just (x,y,z) -> case unify y z of
									Nothing -> Nothing
									Just (S k) -> resolve (S ((helper3 a (Just (x,y,z))) ++ k))

helper3 lst (Just (a,b,c)) = [x|x<-lst,x/=(a,b),x/=(a,c)]++[(a,b)]

cmb::Unifiable a =>Maybe (Subst a) -> Maybe (Subst a) ->Maybe (Subst a)
cmb Nothing _ =Nothing
cmb _ Nothing =Nothing
cmb (Just (S x)) (Just (S y))
	= case unresolved (x ++ y) of
								Nothing -> Just (S (x ++ y))
								Just (g,d,b) -> resolve (S (x ++ y))
	


