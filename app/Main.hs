module Main where

import Parser
import ProofChecker
import Text.Megaparsec.Error

main :: IO ()
main = do
  input <- getContents
  case parse input of
    Left e -> putStrLn $ errorBundlePretty e
    Right proof -> putStrLn $ if checkProof proof then "Correct" else "Incorrect"
