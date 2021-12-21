module Main where

import Data.Char
import Debug.Trace

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
tokenize [] = []
tokenize str@(c:rest) =
  case c of
    '(' -> Open : tokenize rest
    ')' -> Close : tokenize rest
    _ | isSpace c -> tokenize rest
      | otherwise ->
          let (front, back) = splitWhile (\c -> not $ isSpace c || elem c "()") str
          in Str front : tokenize back

data AST = Cons AST AST | Nil | Symbol String
  deriving (Show)

r :: [Token] -> AST
r = snd . go
  where
    go :: [Token] -> ([Token], AST)
    go (Str string:rest) = (rest, Symbol string)
    go (Open:rest) =
      let (rest', globbed) = goGlob rest
      in
      (rest', foldr Cons Nil globbed)

    goGlob :: [Token] -> ([Token], [AST])
    goGlob (Close:rest) = (rest, [])
    goGlob toks =
      let (rest, ast) = go toks
      in
      (ast :) <$> goGlob rest

e :: AST -> AST
e (Cons (Symbol "Cons") rest) = rest
e (Cons (Symbol "Atom") (Cons (Cons _ _) _)) = Nil
e (Cons (Symbol "Atom") (Cons _ _)) = Symbol "T"
e (Cons (Symbol "Cond") rest) =
  let try (Cons (Cons pred (Cons body _)) rest) =
        case e pred of
          Nil -> try rest
          _ -> e body
      try Nil = Nil
      try _ = error "Invalid form passed to Cond"
  in
  try rest
e (Cons (Symbol "Car") (Cons car _)) = car
e (Cons (Symbol "Cdr") (Cons _ cdr)) = cdr
e sym@(Symbol _) = sym
e sym@Nil = sym
e exp = error $ "Got " ++ show exp
--e (Cons (Cons (Symbol "Lambda") names) args) =

p :: AST -> String
p (Cons car cdr) = "(" ++ p car ++ " . " ++ p cdr ++ ")"
p Nil = "()"
p (Symbol str) = str
