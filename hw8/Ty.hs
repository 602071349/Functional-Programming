----------------------------------------------------------------
-- Computer Science 320 (Fall, 2012)
-- Concepts of Programming Languages
--
-- Assignment 8
--   Ty.hs

----------------------------------------------------------------
-- Syntax for Types for the mini-Haskell Interpreter

module Ty (typeCheck, AnnotVal(AnnotVal)) 
  where

import Exp (Exp(..), Oper(..))
import Err
import Env
import Val

data Ty = TyUnit
        | TyVar String
        | TyBool
        | TyInt
        | TyBoolList
        | TyIntList
        | Arrow Ty Ty
  deriving Eq

-- Annotated values, for printing in Main
data AnnotVal = AnnotVal Val Ty
instance Show AnnotVal where
  show (AnnotVal v t) = (show v) ++ " :: " ++ (show t)

-- Useful function for transforming environment (used for
-- applying substitutions to environment).
mapEnv :: (a -> b) -> Env a -> Env b
mapEnv s ((x, t):xts) = (x, s t):(mapEnv s xts)
mapEnv s [] = []

----------------------------------------------------------------
-- Canonical form

data PolyTy = ForAll [String] Ty
showVars (v:vs) = v ++ " "
showVars [v] = v
showVars [] = ""
instance Show PolyTy where
  show (ForAll [] t) = show t
  show (ForAll vs t) = "forall " ++ (showVars vs) ++ "." ++ (show t)

--canon :: Ty -> PolyTy
--Assignment 8, Problem #4 (b) Not Yet Implemented

--freevars :: Ty -> [String]
--Assignment 8, Problem #4 (a) Not Yet Implemented"]

----------------------------------------------------------------
-- This function is exported to the Main module.

-- For testing purposes, you may want to replace the
-- body of typeCheck with a call (ty0 e)

typeCheck :: Exp -> Error Ty
typeCheck e =
  case (ty emptyEnv freshTyVars e) of
    Error msg -> Error msg
    S (t, s, fv)-> S $ s t -- We apply the substitution before
                            -- returning the type.

----------------------------------------------------------------
-- Type-checking Algorithm

tyOp :: Oper -> Ty

tyOp Plus =TyInt `Arrow` ( TyInt `Arrow` TyInt)
tyOp Times=TyInt `Arrow` ( TyInt `Arrow` TyInt) 
tyOp Equal=TyInt `Arrow` ( TyInt `Arrow` TyBool)
tyOp And =TyBool `Arrow` ( TyBool `Arrow` TyBool)
tyOp Or =TyBool `Arrow` ( TyBool `Arrow` TyBool)
tyOp Not =TyBool `Arrow` TyBool
tyOp Head =TyIntList `Arrow` TyInt
tyOp Tail =TyIntList `Arrow` TyInt
tyOp Cons =TyInt `Arrow` (TyIntList `Arrow` TyIntList)
tyOp _ = TyVar "Error" 

ty0 :: Exp -> Error Ty

ty0 Unit =S TyUnit
ty0 Nil =S TyIntList
ty0 (N int)= S TyInt
ty0 (B bool) =S TyBool
ty0 (Op oper) =S (tyOp oper)
ty0 (LamUnit exp) =(ty0 exp)
ty0 (App exp1 exp2) =case ty0 exp1 of
		S (Arrow a b)->case ty0 exp2 of
			S a -> S b
			S d -> Error "Error"
			Error "Error" ->Error "Error"
		S t -> Error "Error"
		Error "Error"->Error "Error"
					
ty0 (If exp1 exp2 exp3) = case ty0 exp1 of
		S TyBool ->case ty0 exp2 of
			S a -> case ty0 exp3 of
					S b -> if a==b then S a else Error "Error"
					Error "Error" -> Error "Error"
			Error "Error" ->Error "Error"							
		S l -> Error "Error"
		Error "Error" -> Error "Error"
ty0 _ =Error "Error"

ty :: Env Ty -> FreshVars -> Exp -> Error (Ty, Subst, FreshVars)
ty gamma fvs Unit =S(TyUnit,idsubst,fvs)
ty gamma fvs Nil =S(TyIntList,idsubst,fvs)
ty gamma fvs (N a) =S(TyInt,idsubst,fvs)
ty gamma fvs (B b) =S(TyBool,idsubst,fvs)
ty gamma fvs (Op op) =S((tyOp op),idsubst,fvs)


ty gamma fvs (Var x) =
  case (findEnv x gamma) of
    Nothing -> Error $ "unbound variable " ++ x
    Just t  -> S (t, idsubst, fvs)

ty gamma fvs (If e1 e2 e3) =
  case (tys gamma fvs [e1, e2, e3]) of
    Error msg -> Error msg
    S ([t1, t2, t3], s, fvs') -> case (unify (s t2) (s t3)) of
								Error "Error" ->Error "cannot infer type"
								S subst ->S ((subst (s t2)),(o s subst),fvs')

ty gamma fvs (App e1 e2) =
  case (tys gamma fvs [e1, e2]) of
    Error msg -> Error msg
    S ([t1, t2], s, (fv:fvs'')) ->
      case (unify (s t1) (Arrow (s t2) fv)) of
        Error msg -> Error msg
        S s'' -> S (s(s'' fv), s `o` s'', fvs'')

ty gamma (fv:fvs') (Lam x e) =
	case (ty (updEnv x fv gamma) fvs' e) of
		Error msg -> Error msg
		S(t,s,fvs'') ->S((fv `Arrow` (s t)),s,fvs'')
  

ty gamma fvs (LamUnit e) =
  case (ty gamma fvs e) of
	Error msg -> Error msg
	S(t,s,fvs) ->S((TyUnit `Arrow` (s t)),s,fvs)
	

ty gamma (fv:fvs') (Let ((x,e):xes) be) =
  case (ty (updEnv x fv gamma) fvs' e) of
    Error msg       -> Error msg
    S (t, s, fvs'') -> ty (updEnv x (s t) gamma) fvs'' (Let xes be)
ty gamma fvs (Let [] be) = ty gamma fvs be

ty gamma fvs e = Error "cannot infer type"

-- This function infers the types of a list of expressions,
-- accumulating the substitutions and returning their
-- composition along with the list of types.
tys :: Env Ty -> FreshVars -> [Exp] -> Error ([Ty], Subst, FreshVars)
tys gamma fvs (e:es) =
  case (tys gamma fvs es) of
    Error msg -> Error msg
    S (ts, s, fvs') ->
      case (ty (mapEnv s gamma) fvs' e) of
        Error msg -> Error msg
        S (t, s', fvs'') -> S (t:ts, \x -> s (s' x), fvs'')
tys gamma fvs [] = S ([], idsubst, fvs)

----------------------------------------------------------------
-- Type Unification

unify :: Ty -> Ty -> Error Subst
unify (TyVar a) (TyVar b)=S (subst a (TyVar b))
unify  q (TyVar a) =S (subst a q)
unify (TyVar a) k =S (subst a k)
unify (Arrow ty1 ty2) (Arrow ty3 ty4)=case (unify ty1 ty3) of
								S a ->case (unify (a ty2) (a ty4)) of
										S b->S (o a b)
										Error "Error" ->Error "Error"
								Error "Error" ->Error "Error"
unify a b = if (a==b) then (S idsubst) else Error "Error"


----------------------------------------------------------------
-- Type Variable Substitutions

type Subst = Ty -> Ty

placeHolder :: String -> Subst
placeHolder s = \t->TyVar s

idsubst :: Subst
idsubst = \x -> x


o :: Subst -> Subst -> Subst
o s1 s2 = \x -> s2 (s1 x) 

subst :: String -> Ty -> Subst
subst str ty = \x -> case x of   
							(TyVar s) -> if str==s then ty else (TyVar s)
							(Arrow a b) -> Arrow (subst str ty a) (subst str ty b)
							k -> k
							 


----------------------------------------------------------------
-- Infinite List of Fresh Type Variables

type FreshVars = [Ty]
freshTyVars :: FreshVars
freshTyVars = helper1 [1..]
helper1 (x:xs)=(TyVar (show x)):(helper1 xs)

----------------------------------------------------------------
-- Printing functions for Syntax of Types

showTy TyUnit      = "()"
showTy (TyVar s)   = s
showTy TyBool      = "Bool"
showTy TyInt       = "Int"
showTy TyBoolList  = "[Bool]"
showTy TyIntList   = "[Int]"
-- If the left argument of an arrow is an arrow, we need to
-- add parentheses to make sure the type is not ambiguous.
showTy (Arrow (Arrow t1 t2) t3) =
   "(" ++ (showTy (Arrow t1 t2)) ++ ") -> " ++ (showTy t3)
showTy (Arrow t1 t2) =
   (showTy t1) ++ " -> " ++ (showTy t2)

instance Show Ty where
  show = showTy
