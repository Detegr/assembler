{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Lib.parse,
      Action(..),
      Instruction(..),
      Value(..)
    ) where

import Debug.Trace
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy as B
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

-- Parser using ByteStrings and using Void as an error type
-- TODO: Add error type?
type Parser = Parsec Void B.ByteString

-- Action type
-- Represents either a valid instruction to execute
-- or an action changing the control flow of the program,
-- for example setting the address or defining labels
data Action = Instruction Instruction | SetExecAddr Value | JumpLabel B.ByteString
  deriving (Show, Eq)

-- Instruction type
-- Contains all instructions that this parser supports
-- along with the arguments that the instructions may take
data Instruction = LDA Value | LDB Value | OUT
  deriving (Show, Eq)

-- Value type
-- An integer containing the information whether the
-- value is a memory address or an immediate value
data Value = Address Integer | Immediate Integer | NoValue
  deriving (Show, Eq)

-- Extracts the Integer value from Value
valueOf :: Value -> Integer
valueOf (Address a) = a
valueOf (Immediate i) = i
valueOf NoValue = 0

-- Instructions that take value
-- For example: LDA 14 or LDB #13
valueInstructions :: [B.ByteString]
valueInstructions = ["LDA", "LDB"]

-- All instructions, containing also the instructions
-- that do not take a value as an argument.
-- For example: LDA 14 or OUT
instructions :: [B.ByteString]
instructions = valueInstructions ++ ["OUT"]

-- Converts ByteString representation of an instruction
-- into an `Instruction` type. Expects that the argument
-- is checked to actually contain a valid instruction
toInstruction :: B.ByteString -> Value -> Maybe Instruction
toInstruction inst value =
    case inst of
      "LDA" -> Just $ LDA value
      "LDB" -> Just $ LDB value
      "OUT" -> case value of
                 NoValue -> Just OUT
                 _ -> Nothing

-- Parser accepting a whitespace or a comment
spaceOrComment :: Parser ()
spaceOrComment = L.space space1 (L.skipLineComment ";") empty

-- Parses one lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceOrComment

-- Parses a specific symbol
symbol :: B.ByteString -> Parser B.ByteString
symbol = L.symbol spaceOrComment

-- Parses an memory address argument
-- It is an unsigned integer
-- For example: 14
address :: Parser Value
address = fmap Address number

-- Parses an immediate value
-- It is an integer (any type) prefixed with '$'
-- It may contain additional prefixes that determine the
-- type of the integer.
-- For example: #$1a or #%101 or #10
immediate :: Parser Value
immediate = symbol "$" >> fmap Immediate number

-- Parses an 8 bit value
-- If no value parser matches, returns NoValue
-- If the value is outside the range 0-255, fails the parser
value8bit :: Parser Value
value8bit = do
  val <- address <|> immediate <|> pure NoValue
  if (valueOf val) > 255
    then fail $ "The argument size must be one byte (0-255)"
    else return val

-- Parses an 16 bit value
-- If no value parser matches, returns NoValue
-- If the value is outside the range 0-255, fails the parser
value16bit :: Parser Value
value16bit = do
  val <- address <|> immediate <|> pure NoValue
  if (valueOf val) > 65535
    then fail $ "The argument size must be two bytes (0-65535)"
    else return val

-- Parses a number in one of following formats:
-- unsigned integer (123)
-- hexadecimal (#1A)
-- binary (%10010)
number :: Parser Integer
number = dec <|> hex <|> binary

-- Parses an unsigned integer
dec :: Parser Integer
dec = lexeme L.decimal

-- Parses a binary value
-- It is an integer containing characters '0' and '1',
-- prefixed with '%'
-- For example: %1011
binary :: Parser Integer
binary = symbol "%" >> lexeme L.binary

-- Parses a hexadecimal value
-- It is a base 16 integer prefixed with '#'
-- For example: #1A
hex :: Parser Integer
hex = symbol "#" >> lexeme L.hexadecimal

-- Parses a string containing a valid instruction
-- Fails if the string can't be found from `instructions`
instruction' :: Parser B.ByteString
instruction' = lexeme (many alphaNumChar >>= check)
  where check instr = let ins = B.pack instr in
                      if ins `elem` instructions
                        then return ins
                        else fail $ show ins ++ " is not a valid instruction"

-- Parses an instruction from the input, along with
-- the possible value argument for the instruction
instruction :: Parser Instruction
instruction = do
  ins <- toInstruction <$> instruction' <*> value8bit
  case ins of
    Just ins -> return ins
    Nothing -> fail $ "Instruction does not take any arguments"

-- Parses an action that changes the current address
-- For example:
-- * = #100
execAddress :: Parser Action
execAddress = symbol "*" >> symbol "=" >> value16bit >>= return . SetExecAddr

-- Parses a label that can be used with jumps
-- A label is a string of alphanumeric characters ending with ':'
-- For example:
-- LOOP:
jumpLabel :: Parser Action
jumpLabel = try $ (fmap B.pack $ manyTill alphaNumChar (symbol ":")) >>= return . JumpLabel

-- Parses an action
-- See `Action` for more details of what the parser supports
action :: Parser Action
action = execAddress <|> jumpLabel <|> fmap Instruction instruction

-- Main parser
-- Parses a list of instructions
parser :: Parser [Action]
parser = between spaceOrComment eof $ many action

-- Compile input into actions
parse :: B.ByteString -> Either String [Action]
parse input =
    case Text.Megaparsec.parse parser "" input of
      Left bundle -> Left $ errorBundlePretty bundle
      Right output -> Right output
