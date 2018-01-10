{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: Implement MCMC via Metropolis Algorithm
-- TODO: Implement stochastic integration

module MarkovChain
  (MarkovChain, TransitionMatrix,
   mkChain, chainFromTM,
   simulateChain, estimateStationary)
  where

import System.Random
import Control.Monad.State
import Numeric.LinearAlgebra

-- import Debug.Trace

import Data.List  (findIndex)
import Data.Maybe (fromJust)

type TransitionMatrix     = Matrix R
type InitialProbabilities = Vector R
type MarkovChainState a g = State (a, g) a
type MarkovChain a g      = (TransitionMatrix, InitialProbabilities, MarkovChainState a g)

mkChain :: (Enum a, RandomGen g) => TransitionMatrix -> InitialProbabilities -> MarkovChain a g
mkChain tm p0 = (tm, p0, mkSampler tm)

-- Make a chain with uniform distribution for p0
chainFromTM :: (Enum a, RandomGen g) => TransitionMatrix -> MarkovChain a g
chainFromTM tm = mkChain tm p0
  where
    n  = length $ toList $ tm ! 0
    p  = 1.0 / fromIntegral n
    p0 = fromList $ replicate n p

mkSampler :: (Enum a, RandomGen g) => TransitionMatrix -> MarkovChainState a g
mkSampler tm = do
  (prevState, gen) <- get;
  let (v :: Double, gen') = random gen;
  let i = indexExtract v (tm ! fromEnum prevState);
  put (toEnum i, gen');
  return $ toEnum i

indexExtract :: Double -> Vector R -> Int
indexExtract v probs =
  fromJust $ findIndex (>= v) intervals
  where
    pl        = toList probs
    intervals = map (\i -> sum $ take i pl) [1..length pl]

simulateChain :: (Enum a, RandomGen g) => MarkovChain a g -> g -> Int -> [a]
simulateChain (_, p0, sampler) gen n =
  reverse $ runner sampler start n
  where
    (v::Double, gen') = random gen
    startState = toEnum $ indexExtract v p0
    start      = (startState, gen')

runner :: (Enum a, RandomGen g)  => MarkovChainState a g -> (a, g) -> Int -> [a]
runner sampler s 0 = [evalState sampler s]
runner sampler s i =
  let
    (o, s') = runState sampler s
  in
    o : runner sampler s' (i - 1)

estimateStationary :: (Enum a, Eq a, RandomGen g) => MarkovChain a g -> g -> Int -> [Double]
estimateStationary chain@(_, p0, _) gen n =
  let
    realisations = simulateChain chain gen n
    uniques      = map toEnum [0..length (toList p0) - 1]
    freqs        = map (\u -> length $ filter (==u) realisations) uniques
    probs        = map toProb freqs
    toProb c     = fromIntegral c / fromIntegral n
  in
    probs
