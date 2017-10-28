module Tree where

import Unify
import Equation


data Tree =Leaf | Node Tree Tree | Var String deriving (Show,Eq)

instance Substitutable Tree where
	subst (S a) Leaf = Leaf
	subst (S a) (Node x y) = Node (subst (S a) x) (subst (S a) y)
	subst (S a) (Var string) = case get string (S a) of
								Nothing -> (Var string)
								Just b -> b
								
instance Unifiable Tree where
	unify Leaf Leaf = Just emp
	unify (Node a b) (Node c d)= cmb (unify a c) (unify b d)
	unify (Var x) (Var y) 
		|x==y = Just emp
		|otherwise = Just (S[(x,(Var y))])
	unify (Var c) x =Just (S [(c,x)])
	unify x (Var c) = Just (S [(c,x)])
	unify _ _ =Nothing
	
	
e0 = Node (Node (Node (Var "x") (Var "y")) (Node (Var "y") (Var "x"))) (Var "z") `Equals` Node (Node (Node Leaf (Var "z")) (Node Leaf (Var "y"))) (Var "x")
e2 = [ (Var "z") `Equals` Leaf , Node (Var "y") Leaf `Equals` Node Leaf (Var "x") , (Var "x") `Equals` Node (Var "z") (Var "z") ]