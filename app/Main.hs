module Main where

import HaskellWorks.Jq.Parser
import System.Environment
import Text.Parsec

data Opts = Opts
  {
  }

defaultOpts :: Opts
defaultOpts = Opts {}

runOpts :: Opts -> [String] -> IO ()
runOpts opts (x:xs) = do
  let expr = parse jqSelector "" x
  print expr
  runOpts opts xs
runOpts _ [] = return ()

main :: IO ()
main = do
  args <- getArgs
  runOpts defaultOpts args
