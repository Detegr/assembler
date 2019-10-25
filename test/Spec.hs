{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Parser

main = hspec $ do
  describe "Instruction parsing" $ do
    it "Parses LDA" $ parse "LDA 0" `shouldBe` Right [Instruction (LDA (Address 0))]
    it "Parses LDB" $ parse "LDB 0" `shouldBe` Right [Instruction (LDB (Address 0))]
    it "Parses OUT" $ parse "OUT" `shouldBe` Right [Instruction OUT]
    it "Does not allow OUT to have an argument" $ isLeft $ parse "OUT 0"
    it "Does not allow LDA to load value over 255" $ isLeft $ parse "LDA 256"
    it "Parses * = addr" $ parse "* = #10" `shouldBe` Right [SetExecAddr (Address 16)]
    it "Parses * = addr where addr > 255" $ parse "* = #1000" `shouldBe` Right [SetExecAddr (Address 4096)]
    it "Does not allow execution address over 65535" $ isLeft $ parse "* = 70000"
