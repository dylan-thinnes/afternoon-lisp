module Main where

import Data.Char
import Debug.Trace
import Text.ParserCombinators.ReadP
import Control.Monad

main :: IO ()
main = putStrLn "Hello, Haskell!"

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile pred as =
  let front = takeWhile pred as
      back = dropWhile pred as
  in
  (front, back)

data Token = Open | Close | Str String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize str = head [match | (match, "") <- readP_to_S parser str]
  where
    parser :: ReadP [Token]
    parser = between skipSpaces skipSpaces $ sepBy (choice [symbol, open, close]) skipSpaces

    symbol, open, close :: ReadP Token
    symbol = Str <$> munch1 isAlphaNum
    open = char '(' >> pure Open
    close = char ')' >> pure Close

data AST = AST ::: AST | Nil | Symbol String
  deriving (Show, Eq)

flatten :: AST -> [AST]
flatten (head ::: tail) = head : flatten tail
flatten tail = [tail]

infixr :::

r :: String -> AST
r str = head [match | (match, "") <- readP_to_S parser str]
  where
    parser :: ReadP AST
    parser = choice [atom, list]

    atom :: ReadP AST
    atom = Symbol . map toLower <$> munch1 isAlphaNum

    list :: ReadP AST
    list = do
      between skipSpaces skipSpaces $ char '('
      elements <- sepBy parser skipSpaces
      between skipSpaces skipSpaces $ char ')'
      pure $ foldr (:::) Nil elements

e :: AST -> AST
e (Symbol "cons" ::: l ::: r ::: Nil) = e l ::: e r
e (Symbol "atom" ::: arg ::: Nil) =
  case e arg of
    _ ::: _ -> Nil
    _ -> Symbol "T"
e (Symbol "cond" ::: rest) =
  let try ((pred ::: body ::: _) ::: rest) =
        case e pred of
          Nil -> try rest
          _ -> e body
      try Nil = Nil
      try branches = error $ "Tried to cond branches: " ++ p branches
  in
  try rest
e (Symbol "car" ::: arg ::: Nil) =
  case e arg of
    l ::: _ -> l
    expr -> error $ "Tried to car expression: " ++ p expr
e (Symbol "cdr" ::: arg ::: Nil) =
  case e arg of
    _ ::: r -> r
    expr -> error $ "Tried to cdr expression: " ++ p expr
e (Symbol "quote" ::: arg ::: Nil) = arg
e sym@(Symbol _) = sym
e sym@Nil = sym
e exp = error $ "Got " ++ show exp

p :: AST -> String
p (car ::: cdr) = "(" ++ p car ++ plist cdr
  where
    plist Nil = ")"
    plist (x ::: rest) = " " ++ p x ++ plist rest
    plist x = " . " ++ p x ++ ")"
p Nil = "()"
p (Symbol str) = str
