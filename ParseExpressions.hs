-- exports a parser for expressions, as well as the convenience functions
-- inParens and inBraces
module ParseExpressions
       ( expr
       , inParens
       , inBraces
       ) where

import Types
import Tokenise

import Text.Parsec
import Text.Parsec.String

-- helper functions

inParens :: Parser a -> Parser a
inParens = between pal par

inBraces :: Parser a -> Parser a
inBraces = between kel ker

-- parser for atoms

atom :: Parser Atom
atom = nat <|> (try rutnCall) <|> var

nat :: Parser Atom
nat = NatAtom <$> natural

-- lambdaAtom :: Parser Atom
-- lambdaAtom = LambdaAtom <$> (inParens $ sepBy varName com)

var :: Parser Atom
var = VarAtom <$> varName

rutnCall :: Parser Atom
rutnCall = RutnCallAtom <$> varName <*> (inParens $ sepBy expr com)

-- parser for terms
term :: Parser Term
term = (try combTerm) <|> atomTerm <|> exprTerm

atomTerm :: Parser Term
atomTerm = Trm <$> atom

combTerm :: Parser Term
combTerm = TrmComb <$> atom <*> hpop <*> term

exprTerm :: Parser Term
exprTerm = ParenTrm <$> (inParens expr)

-- parser for expressions
expr :: Parser Expression
expr = (try combExpr) <|> termExpr

termExpr :: Parser Expression
termExpr = Expr <$> term

combExpr :: Parser Expression
combExpr = ExprComb <$> term <*> lpop <*> expr
