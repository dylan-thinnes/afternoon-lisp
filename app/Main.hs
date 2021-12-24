module Main where

import Data.Char
import Debug.Trace
import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative
import Control.Exception
import System.IO

main :: IO ()
main = repl

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  closed <- isEOF
  if closed
     then putStrLn "Goodbye!"
     else rep >> repl

rep :: IO ()
rep = do
  -- read, eval
  line <- getLine
  result <- try $ evaluate $ e $ r line

  -- print
  case result of
    Left (SomeException e) -> putStrLn $ "ERROR: " ++ displayException e
    Right ast -> putStrLn $ p ast

errIf :: String -> Bool -> Either String ()
errIf mess True = Left mess
errIf _ False = pure ()

(!) = flip errIf

infixr 1 !

data AST = AST ::: AST | Nil | Symbol String
  deriving (Show, Eq)

flatten :: AST -> [AST]
flatten (head ::: tail) = head : flatten tail
flatten tail = [tail]

infixr :::

r :: String -> AST
r str =
  case [match | (match, "") <- readP_to_S parser str] of
    [] -> error "No parse."
    (head:_) -> head
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
e ast
  | Just result <- tryBuiltins ast
  = case result of
      Left err -> error err
      Right ast -> ast
e (Symbol fName ::: args) = error $ "No function with name " ++ show fName
e sym@(Symbol _) = sym
e sym@Nil = sym
e exp = error $ "Got " ++ show exp

data Builtin = Builtin { name :: String, cardinality :: Int, handler :: [AST] -> AST }

run :: AST -> Builtin -> Maybe (Either String AST)
run (Symbol call ::: args) builtin
  | call == name builtin
  = Just $ do
    let argList = flatten args
    let argCount = length argList - 1
    argCount /= cardinality builtin !
      "Wrong number of args for builtin `" ++ call ++ "`\
      \ (got " ++ show argCount ++ ", expected " ++ show (cardinality builtin) ++ ")"
    last argList /= Nil !
      "Non-list args (does not end in Nil)"
    pure $ handler builtin (init argList)
run _ _ = Nothing

builtins :: [Builtin]
builtins = [cons, atom, quote, car, cdr, cond]
  where
  cons', atom', quote', car', cdr' :: [AST] -> AST
  cons' [arg1, arg2] = e arg1 ::: e arg2
  atom' [_ ::: _] = Nil
  atom' [_] = Symbol "T"
  quote' [arg] = arg
  car' [arg] = case e arg of
                 first ::: second -> first
                 _ -> error "car: argument is not a tuple"
  cdr' [arg] = case e arg of
                 first ::: second -> second
                 _ -> error "cdr: argument is not a tuple"
  cond' [conditions] =
    let try ((pred ::: body ::: _) ::: rest) =
          case e pred of
            Nil -> try rest
            _ -> e body
        try Nil = Nil
        try branches = error $ "Tried to cond branches: " ++ p branches
    in
    try conditions

  cons, atom, quote, car, cdr, cond :: Builtin
  cons = Builtin "cons" 2 cons'
  atom = Builtin "atom" 1 atom'
  quote = Builtin "quote" 1 quote'
  car = Builtin "car" 1 car'
  cdr = Builtin "cdr" 1 cdr'
  cond = Builtin "cond" 1 cond'

tryBuiltins :: AST -> Maybe (Either String AST)
tryBuiltins ast = foldr (<|>) Nothing $ map (run ast) builtins

p :: AST -> String
p (car ::: cdr) = "(" ++ p car ++ plist cdr
  where
    plist Nil = ")"
    plist (x ::: rest) = " " ++ p x ++ plist rest
    plist x = " . " ++ p x ++ ")"
p Nil = "()"
p (Symbol str) = str
