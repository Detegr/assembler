module Main where

import Lib

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

writeResultToDisk :: BS.ByteString -> IO ()
writeResultToDisk = BS.putStrLn -- TODO

main :: IO ()
main = do
  args <- getArgs
  case inputFilePath args of
    Just file ->
      BS.readFile file >>= writeResultToDisk . compile
    Nothing -> putStrLn "usage"
