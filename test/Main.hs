{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Test.Hspec
import MyLib
main :: IO ()
main = hspec $ do
    describe "MExpr Parsing" $ do
        it "empty" $ do
            parse "" `shouldBe` Right []
        it "strings" $ do
            parse "\"hello, world\"" `shouldBe` Right [String "hello, world"]
        it "block" $ do
            parse "[1;2;3]" `shouldBe` Right [Block [Number 1, Number 2, Number 3]]
        it "nested block" $ do
            parse "[[];[[]]]" `shouldBe` Right [Block [Block[], Block[Block[]]]]
        it "block with no semicolons" $ do
            parse "[\n\t1\n\t2\n\t3]" `shouldBe` Right [Block[Number 1, Number 2, Number 3]]
        it "block indented optional semicolons" $ do
            parse "[\n\t1\n\t2;3;4\n\t5]" `shouldBe` Right [Block[Number 1, Number 2, Number 3, Number 4, Number 5]]
            parse "[1;2;3;4\n5]" `shouldBe` Right [Block[Number 1, Number 2, Number 3, Number 4, Number 5]]
        it "atom" $ do
            parse "foo" `shouldBe` Right [Atom "foo"]
        it "Compounduences simple" $ do
            parse "a b c" `shouldBe` Right [Compound [Atom "a", Atom "b", Atom "c"]]
        it "Compounduences multiline" $ do
            parse "a\n b\n c\nd\ne\n f\n  g" `shouldBe` Right [Compound [Atom "a",Atom "b",Atom "c"],Atom "d",Compound [Atom "e",Atom "f",Atom "g"]]
    describe "Realistic Parsing" $ do
        it "factorial" $ do
            parse "let factorial = {\n\t1 -> 1\n\tx -> x * factorial (x - 1)}" `shouldBe` Right [Compound [Atom "let", Atom "factorial", Operator "=", Block [Compound [Number 1, Operator "->", Number 1], Compound [Atom "x", Operator "->", Atom "x", Operator "*", Atom "factorial", Block [Compound [Atom "x", Operator "-", Number 1]]]]]]