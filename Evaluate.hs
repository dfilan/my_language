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

-- evaluates an atom
evalAtom :: RutnTable -> ScopeTable -> Atom -> Eval Natural
evalAtom ruTab scTab = \case{
  NatAtom n       -> Right n;
  VarAtom v       -> varLookup v scTab;
  RutnAtom r (eArgs, rArgs) -> do {
    routine   <- rutnLookup ruTab r;
    argValues <- mapM (evalExpr ruTab scTab) eArgs;
    rutnArgs  <- mapM (rutnLookup ruTab) rArgs;
    evalRutn ruTab routine argValues rutnArgs;
    };
  }

-- look up things from scope tables, get something out that's Eval Natural
-- and not Maybe Natural
varLookup :: VarName -> ScopeTable -> Eval Natural
varLookup v tab = case (HM.lookup v tab) of {
  Just n  -> Right n;
  Nothing -> Left "Tried to look up value of non-existent variable.";
  }

-- look up a routine, returning something in Eval Routine rather than Maybe
-- Routine
rutnLookup :: RutnTable -> RutnName -> Eval Routine
rutnLookup tab r = case (HM.lookup r tab) of {
  Just rutn -> Right rutn;
  Nothing   -> Left "Tried to look up non-existent routine."
  }

-- evaluates a term
evalTerm :: RutnTable -> ScopeTable -> Term -> Eval Natural
evalTerm ruTab scTab = \case{
  Trm atom            -> evalAtom ruTab scTab atom;
  TrmComb atom f term -> (liftA2 (hpOpFunc f) (evalAtom ruTab scTab atom)
                          $ evalTerm ruTab scTab term);
  ParenTrm expr       -> evalExpr ruTab scTab expr;
  }

-- evaluate an expression
evalExpr :: RutnTable -> ScopeTable -> Expression -> Eval Natural
evalExpr ruTab scTab = \case{
  Expr term            -> evalTerm ruTab scTab term;
  ExprComb term f expr -> (liftA2 (lpOpFunc f) (evalTerm ruTab scTab term)
                           $ evalExpr ruTab scTab expr);
  }

-- evaluate a block
evalBloc :: RutnTable -> ScopeTable -> Block -> Eval Natural
evalBloc ruTab scTab = \case{
  (Assn v e):sts             -> do {
    newScTab <- updateScope ruTab scTab v e;
    evalBloc ruTab newScTab sts;
    };
  (ITEStmt e sts1 sts2):sts3 -> do {
    val <- evalExpr ruTab scTab e;
    case val of {
      0 -> evalBloc ruTab scTab $ sts2 ++ sts3;
      _ -> evalBloc ruTab scTab $ sts1 ++ sts3;
      };
    };
  (WhileStmt e sts1):sts2    -> do {
    val <- evalExpr ruTab scTab e;
    case val of {
      0 -> evalBloc ruTab scTab sts2;
      _ -> evalBloc ruTab scTab $ sts1 ++ (WhileStmt e sts1):sts2;
      };
    };
  (ReturnStmt e):sts         -> evalExpr ruTab scTab e;
  (RutnDef rName rutn):sts  -> evalBloc (HM.insert rName rutn ruTab) scTab sts;
  []                         -> Left "Tried to evaluate a block with no return\
                                      \ statement.";
  }

-- take an assignment and a scope, and update the scope with the assignment
-- but return an error if the RHS of the assignment is ill-defined
updateScope :: (RutnTable -> ScopeTable -> VarName -> Expression
                -> Eval ScopeTable)
updateScope ruTab scTab v = (fmap (\n -> HM.insert v n scTab)) . (evalExpr ruTab
                                                                  scTab)

-- evaluate a routine call
evalRutn :: RutnTable -> Routine -> [Natural] -> [Routine] -> Eval Natural
evalRutn ruTab (varNames, rutnNames, block) varArgs rutnArgs
 | length varArgs /= length varNames   = Left "Provided the wrong number of\
                                               \ natural-valued arguments to a\
                                               \ routine."
 | length rutnArgs /= length rutnNames = Left "Provided the wrong number of\
                                               \ routine arguments to a\
                                               \ routine (remember, the main\
                                               \ routine can accept no routine\
                                               \ arguments)."
 | otherwise                           = (evalBloc
                                          (HM.union (mkRuTab rutnNames
                                                     rutnArgs)
                                           ruTab)
                                          (mkScTab varNames varArgs)
                                          block)

makeTable :: (Eq k, Hashable k) => [k] -> [v] -> HM.HashMap k v
makeTable ks vs = HM.fromList $ zip ks vs

mkScTab :: [VarName] -> [Natural] -> ScopeTable
mkScTab = makeTable

mkRuTab :: [RutnName] -> [Routine] -> RutnTable
mkRuTab = makeTable

-- to evaluate a program, you get the program and the list of naturals it will
-- be applied to, then evaluate the main routine on that list
-- note: the main routine cannot accept any routine arguments
evalProg :: Program -> [Natural] -> Eval Natural
evalProg (r, ruTab) varArgs = evalRutn ruTab r varArgs []
