{-# LANGUAGE LambdaCase #-}

module EvalTypes
       ( progType
       ) where

import Types

import Control.Monad
import qualified Data.HashMap.Lazy as HM

-- NB we're doing strict typing.

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

typeLookup :: TypeTable -> VarName -> Eval Type
typeLookup table v = case (HM.lookup v table) of
  Just t  -> Right t
  Nothing -> Left "Tried to look up type of undefined variable"

eatType :: Type -> Type -> Eval Type
eatType NatType t           = Left "Applied a routine to too many arguments."
eatType (FuncType t1 t2) t3 = if (t1 == t3)
                              then Right t2
                              else Left ("Expected type " ++ show t1
                                         ++ " but got type " ++ show t3)

eatTypes :: Eval Type -> [Eval Type] -> Eval Type
eatTypes eT = \case
  []     -> eT
  (x:xs) -> eatTypes (join $ eatType <$> eT <*> x) xs

evalBoth :: Eval a -> Eval b -> Eval a
evalBoth (Left x) _  = Left x
evalBoth _ (Left x)  = Left x
evalBoth (Right y) _ = Right y
                                             
staticInsert :: TypeTable -> VarName -> Expression -> Eval TypeTable
staticInsert tab v e = case (HM.lookup v tab) of
  Just type_ -> if newType == Right type_
                then Right tab
                else Left "Tried to change the type of a variable."
  Nothing    -> (\t -> HM.insert v t tab) <$> newType
  where newType = exprType tab e

--------------------------------------------------------------------------------
-- type evaluation functions
--------------------------------------------------------------------------------

atomType :: TypeTable -> Atom -> Eval Type
atomType table = \case
  NatAtom _         -> Right NatType
  LambdaAtom r      -> rutnType table r
  VarAtom v         -> typeLookup table v
  RutnCallAtom v es -> eatTypes (typeLookup table v) $ map (exprType table) es

termType :: TypeTable -> Term -> Eval Type
termType table = \case
  Trm atom            -> atomType table atom
  TrmComb atom f term -> if ((atomType table atom == Right NatType)
                             && (termType table term == Right NatType))
                         then Right NatType
                         else Left "Tried to multiply two terms that weren't\
                                    \ natural numbers."
  ParenTrm expr       -> exprType table expr

exprType :: TypeTable -> Expression -> Eval Type
exprType table = \case
  Expr term            -> termType table term
  ExprComb term f expr -> if ((termType table term == Right NatType)
                              && (exprType table expr == Right NatType))
                          then Right NatType
                          else Left "Tried to add or subtract two terms that\
                                     \ weren't natural numbers."

readRutnType :: Routine -> Type
readRutnType (varsTypes, _, retType) = foldr FuncType retType types
  where types = map snd varsTypes

typeBlock :: Block -> Type -> TypeTable -> Eval TypeTable
typeBlock block retType table = case block of
  []                         -> Right table
  (Assn v e):sts             -> (staticInsert table v e) >>= (typeBlock sts
                                                              retType)
  (ITEStmt e sts1 sts2):sts3 -> if exprType table e == Right NatType
                                then evalBoth (typeBlock (sts1 ++ sts3) retType
                                               table) (typeBlock (sts2 ++ sts3)
                                                       retType table)
                                else Left "Condition for if statement was not\
                                           \ a natural number."
  (WhileStmt e sts1):sts2    -> if exprType table e == Right NatType
                                then typeBlock (sts1 ++ sts2) retType table
                                else Left "Condition for if statement was not\
                                           \ a natural number."
  (ReturnStmt e):sts         -> if exprType table e == Right retType
                                then typeBlock sts retType table
                                else Left "Returned a value of the incorrect\
                                           \ type."

rutnType :: TypeTable -> Routine -> Eval Type
rutnType table rutn = (evalBoth (Right $ readRutnType rutn)
                       (typeBlock block retType newTable))
  where varsTypes = (\(x,_,_) -> x) rutn
        block     = (\(_,y,_) -> y) rutn
        retType   = (\(_,_,z) -> z) rutn
        newTable  = foldr (uncurry HM.insert) table varsTypes

progType :: Program -> Eval ()
progType defs = (fmap (\_ -> ()) $ foldr evalBoth (Right NatType)
                 $ map (rutnType table) rutns)
  where table = HM.fromList $ map (\(v,r) -> (v, readRutnType r)) defs
        rutns = map snd defs
