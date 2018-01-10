-- TODO Improve to detect mismateched brackets & report the error location
-- This program calculates arithemtical expressions over integers using state automatons
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.List as L
import Data.Char as DC
import Data.Maybe as M

import System.Environment
------------------------------------------------------------------------------
data AutomataInput i = AI i | AQ i

class Automata i s | s -> i where
  transitionFunction :: AutomataInput i -> Integer -> s -> s

-- runAutomata is called with a list of inputs & initial state
-- It returns the final state after all inputs are processed
runAutomata :: (Automata i s) => [i] -> Integer -> s -> s
runAutomata []     k s = s
runAutomata (i:[]) k s = transitionFunction (AQ i) k s
runAutomata (i:r)  k s =
  runAutomata r (k + 1) (transitionFunction (AI i) k s)

sublist :: Int -> Int -> [a] -> [a]
sublist a b l =
  take (b - a) $ drop a l

-- expression
-- search start position
-- number of open brackets found thus far
-- number of closed brackets found thus far
findMatchingBracketPosition :: [Char] -> Int -> Integer -> Integer -> Maybe Int
findMatchingBracketPosition expr k o z
  | k >= length expr = Nothing
  | not (elem (expr !! k) "()") = findMatchingBracketPosition expr (k + 1) o z
  | otherwise =
    case (expr !! k) of
      '(' -> findMatchingBracketPosition expr (k + 1) (o + 1) z
      ')' ->
        if (o == z + 1) then Just k
        else if (o > z + 1) then
          findMatchingBracketPosition expr (k + 1) o (z + 1)
        else
          Nothing

splitByBrackets :: [Char] -> [String]
splitByBrackets expr =
  let
    s = L.elemIndex '(' expr
    e = findMatchingBracketPosition expr (M.fromJust s) 0 0
  in
    case (s, e) of
      -- Nasol i pocetna i krajna zagrada
      (Just s', Just e') ->
        let
          a  = take s' expr
          b  = sublist (length a) (e' + 1) expr
          r  = drop (e' + 1) expr
        in
          a : b : splitByBrackets r
      -- Ne nasol nisto znaci stignavme do nivo bez zagradi
      (Nothing, _) -> [expr]
      -- Drugite ne gi proveruvame tuku zemame za gotovo da nema da se slucat

isAtomicExpr :: [Char] -> Bool
isAtomicExpr = M.isNothing . L.elemIndex '('

data Operator = Plus | Minus | Mult | Mod | Div deriving (Eq, Show)
data Term     = TermI Integer | TermOp Operator | TermList [Term]
                  deriving (Eq, Show)

makeOp :: Char -> Term
makeOp chr =
  case chr of
    '+' -> TermOp Plus
    '-' -> TermOp Minus
    '*' -> TermOp Mult
    '%' -> TermOp Mod
    '/' -> TermOp Div

makeNum :: [Char] -> Term
makeNum str =
  TermI (read str :: Integer)

operators = "+-*%/"

makeAtomicTerms :: [Char] -> [Term]
makeAtomicTerms expr =
  let
    (s, r) = span (\k -> not (elem k operators)) expr
  in
    if (s /= "" && r /= "") then
      (makeNum s) : (makeOp (head r)) : makeAtomicTerms (tail r)
    else if (s /= "" && r == "") then
      [makeNum s]
    else if (s == "" && r /= "") then
      makeOp (head r) : makeAtomicTerms (tail r)
    else
      []

makeTerms :: [Char] -> [Term]
makeTerms expr =
  let
    se        = splitByBrackets expr
    bt e      = sublist 1 ((length e) - 1) e -- bracket trim
    f e       =
      if (isAtomicExpr e) then
        makeAtomicTerms e
      else
        [TermList $ makeTerms (bt e)]
  in
    foldr (++) [] (map f se)

data MathExprState  = Zero |
                      Value Integer |
                      ValueOp Integer Operator |
                      Error
                    deriving (Eq, Show)

instance Automata Term MathExprState where
  transitionFunction _ _ Error = Error

  transitionFunction i _ Zero =
      case i of
        AQ (TermI n) -> Value n
        AI (TermI n) -> Value n
        AQ (TermList t) -> runAutomata t 0 Zero
        AI (TermList t) -> runAutomata t 0 Zero
        _ -> Error

  transitionFunction i _ (Value n) =
    case i of
      (AI (TermOp op)) -> ValueOp n op
      _ -> Error

  transitionFunction i _ (ValueOp n op) =
      let
        f t =
          case runAutomata t 0 Zero of
            Error   -> Error
            Value k -> Value $ calculate n op k
      in
      case i of
        AQ (TermI n') -> Value $ calculate n op n'
        AI (TermI n') -> Value $ calculate n op n'

        AQ (TermList t) -> f t
        AI (TermList t) -> f t

        _ -> Error

calculate :: Integer -> Operator -> Integer -> Integer
calculate n Plus  n' = n + n'
calculate n Minus n' = n - n'
calculate n Mult  n' = n * n'
calculate n Mod   n' = mod n n'
calculate n Div   n' = div n n'

evaluate :: [Char] -> MathExprState
evaluate expr =
  runAutomata (makeTerms $ flt expr) 0 Zero
  where
    flt = filter (not . DC.isSpace)
    
main = do
  args <- getArgs
  putStrLn $ show $ evaluate (args !! 0)
