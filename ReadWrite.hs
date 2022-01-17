module ReadWrite where

import Domain
import Data.Char
import Text.Read
import Text.Read.Lex

-- Parsing

isVar x = all isAlpha x && not (x `elem` keywords)
  where keywords = ["zero", "succ", "true", "not", "skip",
                    "for", "if", "then", "else", "while", "do",
                    "true", "false", "hd", "tl"]

when True p = p
when False _ = fail "when failed"

instance Read E where
  readPrec = parens $
             (prec 1 $ do
                e1 <- step readPrec
                Symbol ":" <- lexP
                e2 <- readPrec
                return (Econs e1 e2)) <++
             (prec 1 $ do
                e1 <- step readPrec
                Symbol "<" <- lexP
                e2 <- step readPrec
                return (Elt e1 e2)) <++
             (prec 1 $ do
                e1 <- step readPrec
                Symbol "=" <- lexP
                e2 <- step readPrec
                return (Eeq e1 e2)) <++
             (do
                Number n <- lexP
                when (numberToInteger n == Just 0) $ do
                  return Ezero) <++
             (prec 2 $ do
                Ident "succ" <- lexP
                e <- readPrec
                return (Esucc e)) <++
             (prec 2 $ do
                Ident "pred" <- lexP
                e <- readPrec
                return (Epred e)) <++
             (prec 0 $ do
                Ident "if" <- lexP
                e <- step readPrec
                Ident "then" <- lexP
                e1 <- step readPrec
                Ident "else" <- lexP
                e2 <- readPrec
                return (Eif e e1 e2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  return (Evar x)) <++
             (do
                Ident "true" <- lexP
                return Etrue) <++
             (do
                Ident "false" <- lexP
                return Efalse) <++
             (prec 2 $ do
                Ident "not" <- lexP
                e <- readPrec
                return (Enot e)) <++
             (prec 2 $ do
                Ident "hd" <- lexP
                e <- readPrec
                return (Ehd e)) <++
             (prec 2 $ do
                Ident "tl" <- lexP
                e <- readPrec
                return (Etl e))

instance Read C where
  readPrec = parens $
             (prec 0 $ do
                c1 <- step readPrec
                Punc ";" <- lexP
                c2 <- readPrec
                return (Cseq c1 c2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol ":=" <- lexP
                  e <- reset readPrec
                  return (Cassign x e)) <++
             (do
                Ident "skip" <- lexP
                return Cskip) <++
             (prec 1 $ do
                Ident "if" <- lexP
                e <- readPrec
                Ident "then" <- lexP
                c1 <- step readPrec
                Ident "else" <- lexP
                c2 <- readPrec
                return (Cif e c1 c2)) <++
             (prec 1 $ do
                Ident "for" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cfor e c)) <++
             (prec 1 $ do
                Ident "while" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cwhile e c))