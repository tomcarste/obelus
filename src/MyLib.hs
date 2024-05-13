{-# LANGUAGE OverloadedStrings #-}
module MyLib where

import Data.Scientific (Scientific)
import qualified Data.Char as Char
import Data.String (IsString(..))
import Data.Text hiding (elem)
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
    | Compound [MExpr]
    -- ^ a single item constructed from at least two parts, such as function application or an infix expression
    | Block [MExpr]
    -- ^ a collection of items which may be empty
    deriving (Eq, Ord, Show)

sc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")
scn = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")
lexeme = L.lexeme sc
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
operator = Operator . pack <$> lexeme (some p)
    where p = satisfy (\c -> c /= ';' && (Char.isSymbol c || Char.generalCategory c `elem` [Char.ConnectorPunctuation, Char.DashPunctuation, Char.OtherPunctuation])) <?> "operator"

block :: Parser MExpr
block =
    let body = sepEndBy expr (symbol ";")
        d (op,cl) = Block <$> between op cl body
        i (op,cl) = Block <$> between op cl (many (expr <* optional (symbol ";")))
        brackets = (\ (a,b) -> (symbol a, symbol b)) <$> [("[","]"), ("{","}"), ("(",")")]
        delim = choice (fmap d brackets)
        indent = choice (fmap i brackets)
    in try delim <|> indent

simple = try number <|> try lit <|> try operator <|> atom <|> block

expr :: Parser MExpr
expr = L.lineFold scn $ \sc' ->
  let
    ps :: Parser [MExpr] 
    ps = simple `sepBy1` try sc'
    convert [x] = x
    convert xs = Compound xs
  in convert <$> (ps <* scn) -- (1)
top = many expr
parse str = runParser (top <* eof) "" str 

