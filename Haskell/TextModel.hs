module TextModel
  (getTransitionMatrixForText, charToIndex, indexToChar, validChars, totalValid)
where

import MarkovChain

import qualified Data.Map as M

import qualified Data.List as L

import qualified Numeric.LinearAlgebra as LA

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

import Data.Maybe(fromJust)

validChars :: String
validChars = "abcdefghijklmnopqrstuwxyzABCDEFGHIJKLMNOPQRSTUWXYZ .!?,"

totalValid :: Int
totalValid = length validChars

charToIndex :: Char -> Int
indexToChar :: Int -> Char

charToIndex c = fromJust $ L.elemIndex c validChars
indexToChar i = validChars !! i

onlyValidChars :: String -> String
onlyValidChars = L.filter (`elem` validChars)

getTransitionMatrixForText :: FilePath -> IO TransitionMatrix
getTransitionMatrixForText path = do
  contents  <- readFile path;
  return $ realGetTransitionMatrix (onlyValidChars contents)

{-
realGetTransitionMatrix :: String -> TransitionMatrix
realGetTransitionMatrix contents =
  let
    l      = length contents
    zeros  = replicate (totalValid * totalValid) 0
    zerosM = M.fromList $ zip [0..totalValid*totalValid] zeros
    pairs  = [(contents !! i, contents !! (i + 1)) | i <- [0, l - 2]]

    inc :: (Char, Char) -> M.Map Int Int -> M.Map Int Int
    inc (c1, c2) counts =
      let
        x = charToIndex c1
        y = charToIndex c2
        index = x + y * totalValid
        v = counts M.! index
        v' = v + 1
      in
        M.insert index v' counts

    counts    = foldr inc zerosM pairs
    countsL   = map snd (M.toList counts)
    trans     = listToMatrix (map fromIntegral countsL)

    tvd = fromIntegral totalValid

    countsToProbability :: [Double] -> [Double]
    countsToProbability freq
      | (length . filter (==0.0)) freq == totalValid = replicate totalValid (1.0 / tvd)
      | otherwise                                    = map (\c -> c / sum freq) freq

    trans'    = map countsToProbability trans
  in
    trans'

listToMatrix :: [Double] -> TransitionMatrix
listToMatrix [] = []
listToMatrix ls = row : listToMatrix rest
  where (row, rest) = splitAt totalValid ls
-}
