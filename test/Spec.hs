{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Lexer (lex)
import Regex

data Token = ID String | NUM String | OP String  deriving (Show, Eq)

main :: IO ()
main = hspec $ do
  describe "Lexer.lex" $ do
    it "Tokenizes a simple identifier" $ do
      let rules = [(ID, Plus (Range 'a' 'z'))]
          input = "hello"
          expected = [ID "hello"]
          (nfa, t2t) = translateMany rules
      Lexer.lex nfa t2t input `shouldBe` expected

    it "Tokenizes numbers correctly" $ do
      let rules = [(NUM, Plus (Range '0' '9'))]
          input = "12345"
          expected = [NUM "12345"]
          (nfa, t2t) = translateMany rules
      Lexer.lex nfa t2t input `shouldBe` expected

    it "Handles mixed tokens" $ do
      let rules =
            [ (ID, Plus (Range 'a' 'z'))
            , (NUM, Plus (Range '0' '9'))
            , (OP, Alt (Lit '+') (Lit '-'))
            ]
          input = "a+10-b"
          expected = [ID "a", OP "+", NUM "10", OP "-", ID "b"]
          (nfa, t2t) = translateMany rules
      Lexer.lex nfa t2t input `shouldBe` expected