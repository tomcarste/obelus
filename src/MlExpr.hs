{-# LANGUAGE OverloadedStrings #-}
module MlExpr (MExpr(..), MlExpr.parse, pretty) where

import Data.Scientific (Scientific)
import qualified Data.Char as Char
import Data.List (intercalate)
import Data.String ()
import Data.Text hiding (elem, foldl1, break, concat, unwords, intercalate)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data MExpr
    = Atom Text
    -- ^ a generic indivisible item, such as a name
    | Operator Text
    -- ^ infix item of unknown precedence, parsed flat then reconstructed once we have definitions
    | String Text
    -- ^ a string literal
    | Number Scientific
    -- ^ a numeric literal
    | Block [MExpr]
    -- ^ a collection of items which may be empty
    | Compound [MExpr]
    -- ^ a single item constructed from at least two parts, such as function application or an infix expression
    -- note Compound is left flat for the next phase to match on
    deriving (Eq, Ord, Show)

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: Text -> Parser Text
symbol = L.symbol sc

number :: Parser MExpr
number = Number <$> L.signed (pure ()) (lexeme L.scientific)

lit :: Parser MExpr
lit =
    String . pack
    <$> lexeme ((char '"' *> manyTill L.charLiteral (char '"'))
    <|> (char '\'' *> manyTill L.charLiteral (char '\'')))

atom :: Parser MExpr
atom = Atom . pack <$> lexeme (some p)
    where p = satisfy (\c -> not (c `elem` (";[]{}()"::String) || Char.isSpace c))

operator :: Parser MExpr
operator = Operator . pack <$> lexeme (some p)
    where p = satisfy (\c -> c /= ';' && (Char.isSymbol c || Char.generalCategory c `elem` [Char.ConnectorPunctuation, Char.DashPunctuation, Char.OtherPunctuation])) <?> "operator"

block :: Parser MExpr
block =
    let body = sepEndBy expr (symbol ";")
        d (f,op,cl) = f <$> between op cl body
        comp [x] = x
        comp xs = Compound xs
        brackets = (\ (f,a,b) -> (f,symbol a, symbol b)) <$> [(Block,"[","]"), (Block,"{","}"), (comp, "(",")")]
    in choice (fmap d brackets)

expr :: Parser MExpr
expr = convert <$> ps
  where
    ps = L.lineFold scn $ \sc' ->
        concat <$> (some simple `sepBy1` try sc') <* scn
    simple = try number <|> try lit <|> try operator <|> atom <|> block
    convert [x] = x
    convert xs = Compound xs

parse :: Text -> Either String [MExpr]
parse str =
    case runParser (many (L.nonIndented scn expr) <* eof) "" str of
        Right e -> Right e
        Left err -> Left (errorBundlePretty err)

pretty :: MExpr -> String
pretty (Atom a) = unpack a
pretty (Operator o) = unpack o
pretty (Number s) = show s
pretty (String s) = unpack s
pretty (Compound [e]) = pretty e
pretty (Compound es) =  "(" ++ unwords (fmap pretty es) ++ ")"
pretty (Block es) =
    "[\n\t" ++ intercalate ";\n\t" (fmap pretty es) ++ "\n]"