module Main (main) where

import System.Process (callCommand)

main = do
  callCommand "make test"

  
