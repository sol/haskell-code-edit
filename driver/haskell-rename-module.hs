module Main where

import           System.Environment
import           RenameModule

main :: IO ()
main = do
  [src, dst] <- getArgs
  gitRename src dst
