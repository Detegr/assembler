{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Codegen
import Parser

main = hspec $ do
  describe "Parsing" $ do
    it "Parses LDA" $ parse "LDA 0" `shouldBe` Right [Instruction (LDA (Address 0))]
    it "Parses LDB" $ parse "LDB 0" `shouldBe` Right [Instruction (LDB (Address 0))]
    it "Parses OUT" $ parse "OUT" `shouldBe` Right [Instruction OUT]
    it "Does not allow OUT to have an argument" $ isLeft $ parse "OUT 0"
    it "Does not allow LDA to load value over 255" $ isLeft $ parse "LDA 256"
    it "Parses * = addr" $ parse "* = #10" `shouldBe` Right [SetExecAddr (Address 16)]
    it "Parses * = addr where addr <= 255" $ parse "* = #FA" `shouldBe` Right [SetExecAddr (Address 250)]
    it "Does not allow execution address over 255" $ isLeft $ parse "* = 70000"
    it "Parses byte definitions" $ parse "DB 0 DB #FF" `shouldBe` Right [DB 0, DB 255]
  describe "Codegen" $
    it "Replaces jump labels with absolute addresses" $
      generate [
        SetExecAddr (Address 2),
        JumpLabel "TEST",
        Instruction $ JMP (Jump "TEST")
      ] `shouldBe` pack [0x0, 0x0, 0xFF, 0x2]
