module Main where

import Data.List (dropWhileEnd)
import Lib ( compileHackASM )
import System.Environment ( getArgs )

main :: IO ()
main = do
  filePath <- head <$> getArgs
  asmContent <- readFile filePath

  let byteCode = compileHackASM asmContent
  let outputFilePath = dropWhileEnd (/= '.') filePath ++ "hack"

  writeFile outputFilePath byteCode

  return ()
