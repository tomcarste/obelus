{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Test.Hspec
import MlExpr
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound
var :: String -> Term
var = Var . Unbound.string2Name
record :: [Entry] -> Term
record = Record . flip Unbound.bind () . fromList
main :: IO ()
main = hspec $ do
    describe "MExpr Parsing" $ do
        it "empty" $ do
            parse "" `shouldBe` Right (Block [])
        it "strings" $ do
            parse "\"hello, world\"" `shouldBe` Right (Block [String "hello, world"])
        it "block" $ do
            parse "[1;2;3]" `shouldBe` Right (Block [Number 1, Number 2, Number 3])
        it "nested block" $ do
            parse "[[];[[]]]" `shouldBe` Right (Block [Block[], Block[Block[]]])
        it "atom" $ do
            parse "foo" `shouldBe` Right (Block [Atom "foo"])
        it "Compounds simple" $ do
            let mexpr = parse "a b c" 
            mexpr `shouldBe` Right (Block [Compound [Atom "a", Atom "b", Atom "c"]])
            (mexpr >>= match) `shouldBe` Right (record [Indexed (Unbound.Embed (Apply (Apply (var "a") (var "b")) (var "c")))])
        it "Compounds with blocks" $ do
            let mexpr = parse "f [g a]"
            mexpr `shouldBe` Right (Block [Compound[Atom "f", Block [Compound[Atom "g", Atom "a"]]]])
            (mexpr >>= match) `shouldBe` Right (record [Indexed (Unbound.Embed (Apply (var "f") (record [Indexed (Unbound.Embed (Apply (var "g") (var "a")))])))])
        -- it "Compounds with operators" $ do
        --     let mexpr = parse "f a + g b + h c"
        --     mexpr `shouldBe` Right [Compound[Atom "f", Atom "a", Operator "+", Atom "g", Atom "b", Operator "+", Atom "h", Atom "c"]]
        --     (mexpr >>= traverse match) `shouldBe` Right []
    describe "Realistic Parsing" $ do
        it "prelude" $ do
            let mexpr = parse "id = (A : Type) -> (x : A) -> x"
            mexpr `shouldBe` Right (Block [Compound [Atom "id",Operator "=",Compound [Atom "A",Operator ":",Atom "Type"],Operator "->",Compound [Atom "x",Operator ":",Atom "A"],Operator "->",Atom "x"]])
            (mexpr >>= match) `shouldBe` Right (record [Named (Unbound.string2Name "id") (Unbound.Embed (Lambda (Just Type) (Unbound.bind (Unbound.string2Name "A") (Lambda (Just (var "A")) (Unbound.bind (Unbound.string2Name "x") (var "x"))))))])
        -- it "multiple statements" $ do
        --     let mexpr = parse "let x = 1\nlet y = 2\nx + y"
        --     mexpr `shouldBe` Right [Compound[Atom "let", Atom "x", Operator "=", Number 1], Compound[Atom "let", Atom "y", Operator "=", Number 2], Compound[Atom "x", Operator "+", Atom "y"]]
        --     (mexpr >>= traverse match) `shouldBe` Right []
    --     it "increment" $ do
    --         let mexpr = parse "let incr = x -> x + 1"
    --         mexpr `shouldBe` Right [Compound[Atom "let", Atom "incr", Operator "=", Atom "x", Operator "->", Atom "x", Operator "+", Number 1]]
    --         (mexpr >>= traverse match) `shouldBe` Right []
    --     it "factorial" $ do
    --         let mexpr = parse "let factorial = {\n\t0 -> 1;\n\tx -> x * factorial (x - 1)}"
    --         mexpr `shouldBe` Right [Compound [Atom "let", Atom "factorial", Operator "=", Block [Compound [Number 0, Operator "->", Number 1], Compound [Atom "x", Operator "->", Atom "x", Operator "*", Atom "factorial", Compound [Atom "x", Operator "-", Number 1]]]]]
    --         (mexpr >>= traverse match) `shouldBe` Right 
    --             [ 
    --             ]
    --     it "mutual recursion" $ do
    --         let mexpr = parse "let [isOdd = { 1 -> true; x -> not (isEven (x - 1))}; isEven = {0 -> true; x -> not (isOdd (x - 1))}]"
    --         mexpr `shouldBe` Right [Compound [Atom "let",Block [Compound [Atom "isOdd",Operator "=",Block [Compound [Number 1.0,Operator "->",Atom "true"],Compound [Atom "x",Operator "->",Atom "not",Compound [Atom "isEven",Compound [Atom "x",Operator "-",Number 1.0]]]]],Compound [Atom "isEven",Operator "=",Block [Compound [Number 0.0,Operator "->",Atom "true"],Compound [Atom "x",Operator "->",Atom "not",Compound [Atom "isOdd",Compound [Atom "x",Operator "-",Number 1.0]]]]]]]]
    --         (mexpr >>= traverse match) `shouldBe` Right 
    --             [ 
    --             ]