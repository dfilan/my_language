{-# LANGUAGE TupleSections #-}

-- exports a parser for programs
module Parse
       ( prog
       ) where

import Types
import Tokenise

import Text.Parsec
import Text.Parsec.String

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

inParens :: Parser a -> Parser a
inParens = between pal par

maybeInParens :: Parser a -> Parser a
maybeInParens p = inParens p <|> p

inBraces :: Parser a -> Parser a
inBraces = between kel ker

--------------------------------------------------------------------------------
-- atom parser
--------------------------------------------------------------------------------

atom :: Parser Atom
atom = nat <|> lambdaAtom <|> (try rutnCall) <|> var

nat :: Parser Atom
nat = NatAtom <$> natural

lambdaAtom :: Parser Atom
lambdaAtom = LambdaAtom <$> ((,,)
              <$> (lambda >> (inParens $ sepBy varType com))
              <*> inBraces bloc
              <*> (col >> readType))

varType :: Parser (VarName, Type)
varType = (,) <$> varName <*> (col >> readType)

readType :: Parser Type
readType = maybeInParens $ (try readFuncType) <|> natType

readFuncType :: Parser Type
readFuncType = FuncType <$> readType <*> (arrow >> readType)

var :: Parser Atom
var = VarAtom <$> varName

rutnCall :: Parser Atom
rutnCall = RutnCallAtom <$> varName <*> (inParens $ sepBy expr com)

--------------------------------------------------------------------------------
-- term parser
--------------------------------------------------------------------------------

term :: Parser Term
term = (try combTerm) <|> atomTerm <|> exprTerm

atomTerm :: Parser Term
atomTerm = Trm <$> atom

combTerm :: Parser Term
combTerm = TrmComb <$> atom <*> hpop <*> term

exprTerm :: Parser Term
exprTerm = ParenTrm <$> (inParens expr)

--------------------------------------------------------------------------------
-- expression parser
--------------------------------------------------------------------------------

expr :: Parser Expression
expr = (try combExpr) <|> termExpr

termExpr :: Parser Expression
termExpr = Expr <$> term

combExpr :: Parser Expression
combExpr = ExprComb <$> term <*> lpop <*> expr

--------------------------------------------------------------------------------
-- statement parser
--------------------------------------------------------------------------------

stmt :: Parser Statement
stmt = (
      iteStatement
  <|> whileStatement
  <|> returnStatement
  <|> assignment
      )

assignment :: Parser Statement
assignment = Assn <$> varName <*> (assign >> expr)

iteStatement :: Parser Statement
iteStatement = (try hasElse) <|> justIfThen

hasElse :: Parser Statement
hasElse = (ITEStmt <$> (ifToken >> inParens expr) <*> inBraces bloc
           <*> (elseToken >> inBraces bloc))

justIfThen :: Parser Statement
justIfThen = (ITEStmt <$> (ifToken >> inParens expr) <*> inBraces bloc
              <*> pure [])

whileStatement :: Parser Statement
whileStatement = WhileStmt <$> (whileToken >> inParens expr) <*> inBraces bloc

returnStatement :: Parser Statement
returnStatement = ReturnStmt <$> (returnToken >> expr)

--------------------------------------------------------------------------------
-- parser for blocks
--------------------------------------------------------------------------------

bloc :: Parser Block
bloc = sepEndBy stmt sem

--------------------------------------------------------------------------------
-- parser for routines and their names
--------------------------------------------------------------------------------

rutn :: Parser (VarName, Routine)
rutn = (,) <$> varName <*> ((\(LambdaAtom x) -> x) <$> (assign >> lambdaAtom))

--------------------------------------------------------------------------------
-- parser for programs
--------------------------------------------------------------------------------
prog :: Parser Program
prog = many rutn
