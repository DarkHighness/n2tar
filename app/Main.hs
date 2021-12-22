module Main where

import Data.List (dropWhileEnd)
import Lib
import System.Environment

main :: IO ()
main = do
  filePath <- head <$> getArgs
  asmContent <- readFile filePath

  let byteCode = compileHackASM asmContent
  let outputFilePath = dropWhileEnd (/= '.') filePath ++ "hack"

  writeFile outputFilePath byteCode

  return ()
