module Natural where

import Unify

data Natural = Zero | Succ Natural | Var String deriving (Show,Eq)

instance Substitutable Natural where
	subst (S a) Zero = Zero
	subst (S a) (Succ b) = Succ (subst (S a) b)
	subst (S a) (Var string) 
		= case get string (S a) of 
			Nothing -> (Var string)
			Just b -> b
								
		
instance Unifiable Natural where
	unify Zero Zero = Just emp
	unify (Succ a) (Succ b) = unify a b
	unify (Var c) (Var d) 
		| c==d =Just emp
		|otherwise= Just (S [(c,(Var d))])
	unify (Var c) x = Just (S [(c,x)])
	unify x (Var c) = Just (S [(c,x)])
	unify _ _ = Nothing
		
	