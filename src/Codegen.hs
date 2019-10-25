{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import Data.List (foldl', sortOn)
import Data.List.Split
import Data.Monoid
import Data.Typeable (typeOf)
import Data.Word8
import Debug.Trace
import Parser
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

instructionToBytes :: Instruction -> [Word8]
instructionToBytes instruction =
  case instruction of
    LDA x -> [0x1, fromIntegral $ valueOf x]
    LDB x -> [0x2, fromIntegral $ valueOf x]
    OUT   -> [0x3]

actionToBytes :: BS.ByteString -> Action -> BS.ByteString
actionToBytes acc action =
 case action of
   Instruction inst -> acc `mappend` (BS.pack $ instructionToBytes inst)
   SetExecAddr addr ->
     if padSize > 0
       then mappend acc $ BS.pack . take padSize $ repeat 0
       else BSC.take addrValue acc
     where
       addrValue = fromIntegral . valueOf $ addr
       padSize = fromIntegral $ addrValue - (BS.length acc)
   JumpLabel _ -> acc

generate :: [Action] -> BS.ByteString
generate actions = foldl' actionToBytes "" (concat actionsAscending)
  where actionsByExecAddr = split (keepDelimsL . whenElt $ isSetExecAddr) actions
        actionsAscending = sortOn head actionsByExecAddr
        jumpLabels = filter isJumpLabel actions
