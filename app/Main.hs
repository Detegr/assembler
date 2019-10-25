module Main where

import Parser
import Codegen

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment
import Control.Applicative
import Data.Maybe
import Control.Monad.IO.Class

inputFilePath :: [String] -> Maybe FilePath
inputFilePath args =
  case (length args) of
    0 -> Nothing
    _ -> Just $ head args

writeResultToDisk :: Either String [Action] -> IO ()
writeResultToDisk input =
  case input of
    Left error -> putStrLn error
    Right input -> BS.writeFile "out" $ generate input

main :: IO ()
main = do
  args <- getArgs
  case inputFilePath args of
    Just file ->
      BS.readFile file >>= writeResultToDisk . parse
    Nothing -> putStrLn "usage"
