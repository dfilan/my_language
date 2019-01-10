{-# LANGUAGE LambdaCase #-}

-- Evaluates a list of tokens and a list of natural numbers by forming a program
-- out of it and then evaluating the program.
module Evaluate
       ( evalProg
       ) where

import Types

import Numeric.Natural

import Control.Applicative
import Control.Monad

import qualified Data.HashMap.Lazy as HM

import Data.Hashable

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

-- look up things from scope tables, get something out that's Eval Natural
-- and not Maybe Natural
myLookup :: VarName -> ScopeTable -> Eval Value
myLookup v tab = case (HM.lookup v tab) of
  Just val -> Right val
  Nothing  -> (Left $ "Tried to look up value of non-existent variable '" ++ v
               ++ "'.")

-- take an assignment and a scope, and update the scope with the assignment
-- but return an error if the RHS of the assignment is ill-defined
updateWithExpr :: ScopeTable -> VarName -> Expression -> Eval ScopeTable
updateWithExpr table v = (fmap (\x -> HM.insert v x table)) . (evalExpr table)

-- take a scope and a list of variable-value pairs, and update the scope with
-- the variable-value pairs
addList :: ScopeTable -> [(VarName, Value)] -> ScopeTable
addList table list = HM.union (HM.fromList list) table

--------------------------------------------------------------------------------
-- functions that evaluate various things
--------------------------------------------------------------------------------

-- evaluates an atom
evalAtom :: ScopeTable -> Atom -> Eval Value
evalAtom table = \case
  NatAtom n         -> Right $ NatValue n
  LambdaAtom r      -> Right $ RutnValue r
  VarAtom v         -> myLookup v table
  RutnCallAtom v es -> case myLookup v table of
    Left msg            -> Left msg
    Right (NatValue n)  -> Left "Natural numbers cannot take arguments."
    Right (RutnValue r) -> mapM (evalExpr table) es >>= evalRutn table r


-- evaluates a term
evalTerm :: ScopeTable -> Term -> Eval Value
evalTerm table = \case
  Trm atom            -> evalAtom table atom
  TrmComb atom f term -> (liftA2 (hpOpFunc f) (evalAtom table atom)
                          $ evalTerm table term)
  ParenTrm expr       -> evalExpr table expr

-- evaluates an expression
evalExpr :: ScopeTable -> Expression -> Eval Value
evalExpr table = \case
  Expr term            -> evalTerm table term
  ExprComb term f expr -> (liftA2 (lpOpFunc f) (evalTerm table term)
                           $ evalExpr table expr)

-- evaluate a block
evalBloc :: ScopeTable -> Block -> Eval Value
evalBloc table = \case
  (Assn v e):sts             -> updateWithExpr table v e >>= (flip evalBloc sts)
  (ITEStmt e sts1 sts2):sts3 -> do
    val <- evalExpr table e
    evalBloc table $ case val of
      NatValue 0 -> sts2 ++ sts3
      _          -> sts1 ++ sts3
  (WhileStmt e sts1):sts2    -> do
    val <- evalExpr table e
    evalBloc table $ case val of
      NatValue 0 -> sts2
      _          -> sts1 ++ (WhileStmt e sts1):sts2
  (ReturnStmt e):_           -> evalExpr table e
  []                         -> Left "Tried to evaluate a block with no return\
                                      \ statement."

-- evaluate a routine call
evalRutn :: ScopeTable -> Routine -> [Value] -> Eval Value
evalRutn table (varsTypes, block, retType) vals
 | numVals > numArgs  = Left "Applied too many arguments to a routine."
 | numVals == numArgs = evalBloc (addList table argsVals) block
 | numVals < numArgs  = let assignments = zipWith assign argNames vals
                        in Right $ RutnValue (drop numVals varsTypes,
                                              assignments ++ block, retType)
 where numVals  = length vals
       numArgs  = length varsTypes
       argNames = map fst varsTypes
       argsVals = zip argNames vals

assign :: VarName -> Value -> Statement
assign name val = Assn name $ case val of
  NatValue n  -> Expr $ Trm $ NatAtom n
  RutnValue r -> Expr $ Trm $ LambdaAtom r

-- to evaluate a program, you get the program and the list of naturals it will
-- be applied to, then evaluate the main routine on that list
-- note: the main routine cannot accept any routine arguments, and must return
-- a natural number
evalProg :: Program -> [Natural] -> Eval Natural
evalProg p ns = do
  mainRutn <- myLookup "main" scope
  value <- evalRutn scope ((\(RutnValue r) -> r) mainRutn) args
  return $ (\(NatValue n) -> n) value
  where scope = HM.fromList $ map (\(v,r) -> (v, RutnValue r)) p
        args  = map NatValue ns
