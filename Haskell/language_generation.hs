module Main where

import qualified MarkovChain as MC
import qualified TextModel   as TM

import System.Random

gen :: StdGen
n :: Int

gen        = mkStdGen 0
n          = 10000

inTextFile :: String
outTextFile :: String

inTextFile   = "sometext.txt"
outTextFile  = "output.txt"

main :: IO ()
main = do
  tm <- TM.getTransitionMatrixForText inTextFile
  let ch   = MC.chainFromTM tm
  let s    = MC.simulateChain ch gen n
  let stat = MC.estimateStationary ch gen n
  let text = map TM.indexToChar s;
  writeFile outTextFile text
