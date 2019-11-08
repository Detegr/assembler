{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import Data.List (foldl', sortOn, uncons, length)
import Data.List.Split
import Data.HashMap hiding (filter, null)
import Data.Monoid
import Data.Typeable (typeOf)
import Data.Word8
import Debug.Trace
import Parser
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

-- Converts an instruction to one or more bytes of machine code
instructionToBytes :: Instruction -> [Word8]
instructionToBytes instruction =
  case instruction of
    LDA (Address x)   -> [0x1,  fromIntegral x]
    LDA (Immediate x) -> [0x10, fromIntegral x]
    LDB (Address x)   -> [0x2,  fromIntegral x]
    LDB (Immediate x) -> [0x20, fromIntegral x]
    OUT               -> [0x3]
    JMP x             -> [0xFF, fromIntegral $ valueOf x]

-- Returns the number of bytes a machine code representation of
-- an instruction occupies
instructionSize :: Instruction -> Integer
instructionSize = fromIntegral . length . instructionToBytes

-- Collects jump labels from a list of actions (in ascending order by execution address)
-- Returns a hashmap where the key is the jump label and the value is the absolute address
-- of where the label points
associateLabelsToAddresses :: [Action] -> Map BS.ByteString Integer
associateLabelsToAddresses = go 0 empty
  where go addr ret actions = case uncons actions of
                                Just (maybeExecAddr, rest) ->
                                  case maybeExecAddr of
                                    SetExecAddr addr -> go (valueOf addr) ret rest
                                    JumpLabel label -> go addr (insert label addr ret) rest
                                    Instruction inst -> go (addr + instructionSize inst) ret rest
                                    DB _ -> go (addr + 1) ret rest
                                Nothing -> ret

-- Generates machine code from a list of actions
-- The algorithm is following:
--   - Split the list of actions at `SetExecAddr` elements, leaving them as the first element in the array
--   - Sort the resulting list by the first element (execution address)
--   - Resolve absolute locations of jump labels using the sorted list of instructions
--   - Convert the resulting list into machine code, replacing jump instructions with absolute jumps
generate :: [Action] -> BS.ByteString
generate actions = foldl' (actionToBytes labelsToAddresses) "" actionsAscending
  where actionsByExecAddr = filter (not . null) $ split (keepDelimsL . whenElt $ isSetExecAddr) actions
        actionsAscending = concat $ sortOn head actionsByExecAddr
        labelsToAddresses = associateLabelsToAddresses actionsAscending
        actionToBytes labels acc action =
          case action of
            Instruction inst ->
              let absoluteAddr label = Address $ labels ! label
                  inst' = case inst of
                            -- TODO: Make a pretty error if `label` refers to a nonexisting label
                            LDA (Jump label) -> LDA $ absoluteAddr label
                            LDB (Jump label) -> LDB $ absoluteAddr label
                            JMP (Jump label) -> JMP $ absoluteAddr label
                            inst -> inst
                          in
                            acc `mappend` BS.pack (instructionToBytes inst')
            SetExecAddr addr ->
              if padSize > 0
                then mappend acc $ BS.pack . take padSize $ repeat 0
                else BSC.take addrValue acc
              where
                addrValue = fromIntegral . valueOf $ addr
                padSize = fromIntegral $ addrValue - BS.length acc
            JumpLabel _ -> acc
            DB val -> acc `mappend` BS.pack [fromIntegral val]
