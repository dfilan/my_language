-- TODO: rewrite Evaluate to use the reader monad
-- TODO: add for loops
-- TODO: use chainl1 instead of recursion in my expression grammar
-- note: amazingly, this only works if I make expressions, terms, and atoms
-- the same thing.
-- TODO?: use List1 in cases where I promise I don't have an empty list
-- TODO: allow comments

-- all the types we'll be exporting to other files
-- note: the structure of some of these types closely relates to the grammar we
-- will allow
module Types
       ( LowPrioOp(..)
       , HighPrioOp(..)
       , lpOpFunc
       , hpOpFunc
       , VarName
       , ReservedName(..)
       , Symbol(..)
       , ScopeTable
       , Program
       , Routine
       , Block
       , Statement(..)
       , Expression(..)
       , Term(..)
       , Atom(..)
       , Eval
       , Type(..)
       , Value(..)
       , TypedValue
       ) where

import Numeric.Natural

import qualified Data.HashMap.Lazy as HM

-- data types for operations by how much precedence they have
data LowPrioOp = Plus | Monus deriving (Eq, Show)
data HighPrioOp = Times deriving (Eq, Show)

-- functions to take operators and return the operations that they represent
lpOpFunc :: LowPrioOp -> Natural -> Natural -> Natural
lpOpFunc Plus m n  = m + n
lpOpFunc Monus m n
    | m > n     = m - n
    | otherwise = 0

hpOpFunc :: HighPrioOp -> Natural -> Natural -> Natural
hpOpFunc Times m n = m * n

--------------------------------------------------------------------------------
-- data types representing things that can be in programs ----------------------
--------------------------------------------------------------------------------

-- defining a data type for variable names.
-- secretly, they're actually going to be strings that consist entirely of
-- alphabetic unicode characters that don't start with upper or title case
type VarName = String

data ReservedName = If
                  | Else
                  | While
                  | Return
                  deriving (Eq, Show)

data Symbol = Assign -- assignment sign, ':='
            | Sem    -- semicolon, ';'
            | Pal    -- left paren, '('
            | Par    -- right paren, ')'
            | Kel    -- left brace, '{'
            | Ker    -- right brace, '}'
            | Com    -- comma, ','
            | Lambda -- lambda, '\'
            | Col    -- colon, ':'
            | Arrow  -- '->'

-- while we're interpreting the program, we're going to keep a scope table
-- associating variable names with the values that they hold
type ScopeTable = HM.HashMap VarName TypedValue

--------------------------------------------------------------------------------
-- data types representing the grammar of programs that we accept --------------
--------------------------------------------------------------------------------

-- a program is just a list of all globally-defined routine names and the
-- routines that they represent.
-- one of the routines should be called main, and that routine will be the one
-- that actually gets evaluated
type Program = [(VarName, Routine)]

-- a routine is a list of the routine's arguments and their types,
-- the body of the function,
-- and a return type.
type Routine = ([(VarName, Type)], Block, Type)

-- a block is just a bunch of statements, one of which should probably be a
-- return statement
type Block = [Statement]

data Statement  = Assn VarName Expression
                | ITEStmt Expression Block Block
                | WhileStmt Expression Block
                | ReturnStmt Expression
                deriving (Eq, Show)

data Expression = Expr Term
                | ExprComb Term LowPrioOp Expression
                deriving (Eq, Show)

data Term       = Trm Atom
                | TrmComb Atom HighPrioOp Term
                | ParenTrm Expression -- an expression with parens on either
                deriving (Eq, Show)   -- side

data Atom       = NatAtom Natural
                | LambdaAtom Routine
                | VarAtom VarName
                | RutnCallAtom VarName [Expression]
                deriving (Eq, Show)

--------------------------------------------------------------------------------
-- data types that represent evaluations of things -----------------------------
--------------------------------------------------------------------------------

-- data type that allows for errors that can show up during execution. Because
-- of the structure of the either type, somebody running an invalid program
-- will only see the first error that shows up during execution
type Eval a = Either String a

-- types that things in the program can have
data Type = NatType
          | FuncType Type Type
          deriving (Eq, Show)

-- values that things in the program can take on
data Value = NatValue Natural
           | RutnValue Routine

-- type-value pairs, which will be stored in the scope table
type TypedValue = (Type, Value)
