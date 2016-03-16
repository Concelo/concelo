module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Random
import System.Exit
import System.Random.TF
import System.Process (callCommand)
import Text.Printf

import qualified Database.Concelo.Simulation as Simulation
import qualified Data.Tree.RBTests as RBTests

check description prop = do
  printf "%-25s: " description

  result <- quickCheckWithResult
    stdArgs {- replay = Just (QCGen $ seedTFGen (0, 0, 0, 0), 0) -} prop

  unless (isSuccess result) $ do
    putStrLn $ "Use " ++ show (usedSeed result) ++ " as the initial seed"
    putStrLn $ "Use " ++ show (usedSize result) ++ " as the initial size"
    exitFailure

main = do
  Simulation.runTests check

  RBTests.runTests check

  -- callCommand "make test"
