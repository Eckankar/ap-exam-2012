{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
--
-- Soil parser
-- Exam for Advanced Programming, B1-2012
-- Sebastian Paaske Tørholm <sebbe@diku.dk>
--

module SoilParser
  ( parseString
  , parseFile
  , Error(..)
  ) where

import SoilAst
import Control.Monad (ap, MonadPlus(mzero, mplus), liftM)
import Control.Applicative ((<$>), Applicative(..),
                            Alternative(empty, (<|>)))
import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isAlpha)

-- Instances for Applicative ReadP and Alternative ReadP aren't in base-4.5
instance Applicative ReadP where
    pure = return
    (<*>) = ap

instance Alternative ReadP where
    empty = mzero
    (<|>) = mplus

{- AmbiguousParseTree - there are multiple possible interpretations of the
                        code.
   InvalidProgram     - there's a syntax error. -}

data Error = AmbiguousParseTree
           | InvalidProgram
    deriving (Show, Eq)

parenthesized :: ReadP a -> ReadP a
parenthesized = between (schar '(') (schar ')')

-- Char preceded by whitespace
schar :: Char -> ReadP Char
schar c = skipSpaces >> char c

-- String preceded by whitespace
sstring :: String -> ReadP String
sstring s = skipSpaces >> string s

keyword :: String -> Bool
keyword = flip elem ["let", "from", "end", "case", "of", "if", "then", "else",
                     "send", "create", "become", "self"]

program :: ReadP Program
program = do d <- defops
             skipSpaces
             eof
             return d

defops :: ReadP Program
defops = (do f <- fundef
             (fs, as) <- defops
             return (f:fs, as))
     <|> do as <- actops
            return ([], as)

fundef :: ReadP Func
fundef = do sstring "let"
            i    <- ident
            ps   <- parenthesized pars
            sstring "from"
            n    <- name
            schar '='
            ex   <- expr
            sstring "end"
            return $ Func i ps n ex

pars :: ReadP [Name]
pars = sepBy name (schar ',')

expr :: ReadP Expr
expr = fmap Acts actops
   <|> (do sstring "case"
           p <- prim
           sstring "of"
           (cs, def) <- cases
           sstring "end"
           return $ CaseOf p cs def)
   <|> do sstring "if"
          p1 <- prim
          sstring "=="
          p2 <- prim
          sstring "then"
          ethen <- expr
          sstring "else"
          eelse <- expr
          sstring "end"
          return $ IfEq p1 p2 ethen eelse

cases :: ReadP ([([Name], Expr)], Expr)
cases = (do ps <- parenthesized pars
            schar ':'
            e  <- expr
            (cs, def) <- cases
            return ((ps, e) : cs, def))
    <|> ([],) <$> (schar '_' >> schar ':' >> expr)

actops :: ReadP [ActOp]
actops = many actop

actop :: ReadP ActOp
actop = (do sstring "send"
            as <- parenthesized args
            sstring "to"
            p  <- prim
            return $ SendTo as p)
    <|> (do sstring "create"
            p  <- prim
            sstring "with"
            (fcp, fca) <- fcall
            return $ Create p fcp fca)
    <|> do sstring "become"
           (fcp, fca) <- fcall
           return $ Become fcp fca

fcall :: ReadP (Prim, [Prim])
fcall = do p <- prim
           a <- parenthesized args
           return (p, a)

args :: ReadP [Prim]
args = skipSpaces >> sepBy prim (schar ',')

prim :: ReadP Prim
prim = chainl1 primBasic (sstring "concat" >> return Concat)

primBasic :: ReadP Prim
primBasic = fmap Id ident
        <|> fmap Par name
        <|> (sstring "self" >> return Self)

ident :: ReadP Ident
ident = schar '#' >> munch1((||) <$> isAlphaNum <*> (==) '_')

name :: ReadP Name
name = do skipSpaces
          n  <- satisfy isAlpha
          ns <- munch((||) <$> isAlphaNum <*> (==) '_')
          case n:ns of
            name' | keyword name' -> pfail
                  | otherwise     -> return name'

parseString :: String -> Either Error Program
parseString s = case readP_to_S program s of
                    []       -> Left InvalidProgram
                    [(p,"")] -> Right p
                    _        -> Left AmbiguousParseTree

parseFile :: FilePath -> IO (Either Error Program)
parseFile = liftM parseString . readFile
