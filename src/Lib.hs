module Lib (compileHackASM) where

import Control.Monad (void)
import qualified Control.Monad.Trans.State as ST
import Data.Char (intToDigit, isDigit, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import Data.Sequence (takeWhileR)
import Data.Void (Void)
import Numeric (showIntAtBase)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    choice,
    oneOf,
    optional,
    runParser,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, string)

type Code = String

type BitCode = String

type Symbol = String

type SymbolMap = M.Map Symbol Int

type CompileState = ST.State SymbolMap [BitCode]

data Env = Env Int SymbolMap

type Parser = Parsec Void String

data Jump = JGT | JEQ | JGE | JLT | JNE | JLE | JMP deriving (Show, Eq)

data Dest = M | D | MD | A | AM | AD | AMD deriving (Show, Eq)

data Instruction = Instruction (Maybe Dest) String (Maybe Jump) deriving (Show, Eq)

lineNumber :: Env -> Int
lineNumber (Env x _) = x

varIndex :: Env -> Int
varIndex (Env x _) = x

symbolMap :: Env -> SymbolMap
symbolMap (Env _ y) = y

preprocessCode :: Code -> [Code]
preprocessCode = filter (not . null) . map reformat . lines
  where
    reformat :: Code -> Code
    reformat [] = []
    reformat ('/' : '/' : xs) = []
    reformat (x : xs) | isSpace x = reformat xs
    reformat (x : xs) = x : reformat xs

predefinedSymbols :: SymbolMap
predefinedSymbols = M.fromList (registerSymbol ++ otherSymbol)
  where
    registerSymbol = map (\x -> ('R' : show x, x)) [0 .. 16]
    otherSymbol = [("SCREEN", 16384), ("KBD", 24576), ("SP", 0), ("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4)]

parseCode :: [Code] -> ([(Int, Code)], SymbolMap)
parseCode code =
  let (x, y) = ST.runState (parse' code) (Env 0 predefinedSymbols)
   in (filter (\(l', _) -> l' /= -1) x, symbolMap y)
  where
    parse' :: [Code] -> ST.State Env [(Int, Code)]
    parse' = mapM go

    go c@('(' : xs) = do
      let name = takeWhile (/= ')') xs

      st <- ST.get

      let cl = lineNumber st
      let sm = symbolMap st

      case M.lookup name sm of
        Just _ -> return (cl, c)
        Nothing ->
          let sm' = M.insert name cl sm
           in do
                ST.put $ Env cl sm'
                return (-1, c)
    go c = do
      st <- ST.get

      let cl' = lineNumber st + 1
      let sm = symbolMap st

      ST.put $ Env cl' sm

      return (cl', c)

parseDest :: Parser Dest
parseDest =
  choice
    [ MD <$ string "MD",
      M <$ string "M",
      D <$ string "D",
      AMD <$ string "AMD",
      AM <$ string "AM",
      AD <$ string "AD",
      A <$ string "A"
    ]

parseJump :: Parser Jump
parseJump =
  choice
    [ JGT <$ string "JGT",
      JEQ <$ string "JEQ",
      JGE <$ string "JGE",
      JLT <$ string "JLT",
      JNE <$ string "JNE",
      JLE <$ string "JLE",
      JMP <$ string "JMP"
    ]

parseInstruction :: Parser Instruction
parseInstruction = do
  dest <- optional . try $ do
    dest <- parseDest
    void (char '=')
    return dest
  comp <- some (alphaNumChar <|> oneOf "-!+-&|")
  jump <- optional . try $ do
    void (char ';')
    jump <- parseJump
    return jump
  eof
  return $ Instruction dest comp jump

emitCode :: Maybe Dest -> String -> Maybe Jump -> String
emitCode dest comp jump = "111" ++ emitComp comp ++ emitDest dest ++ emitJump jump
  where
    emitDest :: Maybe Dest -> String
    emitDest Nothing = "000"
    emitDest (Just x) = case x of
      M -> "001"
      D -> "010"
      MD -> "011"
      A -> "100"
      AM -> "101"
      AD -> "110"
      AMD -> "111"
    emitJump :: Maybe Jump -> String
    emitJump Nothing = "000"
    emitJump (Just x) = case x of
      JGT -> "001"
      JEQ -> "010"
      JGE -> "011"
      JLT -> "100"
      JNE -> "101"
      JLE -> "110"
      JMP -> "111"
    emitComp :: String -> String
    emitComp comp = emitPrefix comp ++ emitSuffix comp

    emitPrefix :: String -> String
    emitPrefix x = if 'M' `elem` x then "1" else "0"

    emitSuffix :: String -> String
    emitSuffix "0" = "101010"
    emitSuffix "1" = "111111"
    emitSuffix "-1" = "111010"
    emitSuffix "D" = "001100"
    emitSuffix "A" = "110000"
    emitSuffix "M" = "110000"
    emitSuffix "!D" = "001101"
    emitSuffix "!A" = "110001"
    emitSuffix "!M" = "110001"
    emitSuffix "-D" = "001111"
    emitSuffix "-A" = "110011"
    emitSuffix "-M" = "110011"
    emitSuffix "D+1" = "011111"
    emitSuffix "A+1" = "110111"
    emitSuffix "M+1" = "110111"
    emitSuffix "D-1" = "001110"
    emitSuffix "A-1" = "110010"
    emitSuffix "M-1" = "110010"
    emitSuffix "D+A" = "000010"
    emitSuffix "D+M" = "000010"
    emitSuffix "D-A" = "010011"
    emitSuffix "D-M" = "010011"
    emitSuffix "A-D" = "000111"
    emitSuffix "M-D" = "000111"
    emitSuffix "D&A" = "000000"
    emitSuffix "D&M" = "000000"
    emitSuffix "D|A" = "010101"
    emitSuffix "D|M" = "010101"
    emitSuffix _ = error "illegal comp"

compileCode :: [(Int, Code)] -> SymbolMap -> [BitCode]
compileCode code sym = ST.evalState (compile' code) (Env 16 sym)
  where
    compile' :: [(Int, Code)] -> ST.State Env [BitCode]
    compile' = mapM go

    go (_, '@' : xs) | all isDigit xs = (return . pad16 . toBin . read) xs
    go (_, '@' : xs) = do
      m <- ST.get
      let sym = symbolMap m
      pad16 . toBin <$> case M.lookup xs sym of
        Nothing -> do
          let idx = varIndex m
          let sym' = M.insert xs idx sym
          ST.put $ Env (idx + 1) sym'
          return idx
        Just idx -> do
          return idx
    go (l, xs) = do
      let ins = runParser parseInstruction xs xs
      case ins of
        Left e -> error $ show e
        Right (Instruction dest comp jump) -> return $ emitCode dest comp jump

compile :: Code -> [BitCode]
compile = uncurry compileCode . parseCode . preprocessCode

pad16 :: BitCode -> BitCode
pad16 x = replicate (16 - length x) '0' ++ x

toBin :: Int -> BitCode
toBin n = showIntAtBase 2 intToDigit n ""

compileHackASM :: Code -> BitCode
compileHackASM = unlines . compile
