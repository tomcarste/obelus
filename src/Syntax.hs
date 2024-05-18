{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Syntax (Name, Type, Term(..), match, toMExpr, prettyPrint) where
import Data.Function (on)
import Data.Text (unpack, pack)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound
import MlExpr

type Name = Unbound.Name Term
type Type = Term
data Term
    = Var Name
    | Apply Term Term
    | Lambda Type (Unbound.Bind Name Term)
    | Pi Type (Unbound.Bind Name Type)
    | Type
    | Let Name Type Term
    | Seq [Term]
    | Void
    deriving (Show, Generic, Unbound.Subst Term)
instance Unbound.Alpha Term where
    aeq' :: Unbound.AlphaCtx -> Term -> Term -> Bool
    aeq' ctx = Unbound.gaeq ctx `on` from
instance Eq Term where
    (==) = Unbound.aeq

match :: MExpr -> Either String Term
match (Atom "Type") = Right Type
match (Atom a) = Right . Var . Unbound.string2Name $ unpack a
match (Operator o) = Right . Var . Unbound.string2Name $ unpack o
match (String _) = Left "unimplemented"
match (Number _) = Left "unimplemented"
match (Compound [Atom "let", Atom n, Operator ":", t, Operator "=", e]) = do
    ttype <- match t
    eterm <- match e
    Right $ Let (Unbound.string2Name $ unpack n) ttype eterm
match (Compound [Compound [Atom a, Operator ":", t], Operator "->", b]) = do
    ttype <- match t
    bterm <- match b
    Right $ Lambda ttype (Unbound.bind (Unbound.string2Name $ unpack a) bterm)
match (Compound [Compound [Atom a, Operator ":", t], Operator "=>", b]) = do
    ttype <- match t
    utype <- match b
    Right $ Pi ttype (Unbound.bind (Unbound.string2Name $ unpack a) utype)
match (Compound es) = traverse match es >>= Right . foldl1 Apply
match (Block es) = traverse match es >>= Right . Seq

toMExpr :: Unbound.Fresh m => Term -> m MExpr
toMExpr Type = pure $ Atom "Type"
toMExpr (Var n) = pure . Atom . pack $ Unbound.name2String n
toMExpr (Apply l r) = do
    lm <- toMExpr l
    rm <- toMExpr r
    pure $ Compound [lm, rm]
toMExpr (Lambda t a) = do
    (x,b) <- Unbound.unbind a
    texpr <- toMExpr t
    bexpr <- toMExpr b
    pure $ Compound 
            [ Compound [ Atom . pack $ Unbound.name2String x, Operator ":", texpr ]
            , Operator "->"
            , bexpr
            ]
toMExpr (Pi t a) = do
    (x,u) <- Unbound.unbind a
    texpr <- toMExpr t
    uexpr <- toMExpr u
    pure $ Compound 
            [ Compound [ Atom . pack $ Unbound.name2String x, Operator ":", texpr]
            , Operator "=>"
            , uexpr
            ]
toMExpr (Let x t e) = do
    texp <- toMExpr t
    eexp <- toMExpr e
    pure $ Compound[Atom "let", Atom . pack $ Unbound.name2String x, Operator ":", texp, Operator "=", eexp] 
toMExpr (Seq es) = do
    esexp <- traverse toMExpr es
    pure $ Block esexp
toMExpr Void = pure (Atom "")
    
prettyPrint :: Term -> String
prettyPrint = pretty . Unbound.runFreshM . toMExpr 