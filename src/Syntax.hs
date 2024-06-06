{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Syntax where

import MlExpr
import Data.Functor ((<&>))
import Data.Foldable (foldrM)
import Data.Text (Text, pack)
import Control.Monad.Reader
import Control.Monad.State
import Data.Scientific

type Atom = (Text, Int)
data Local
    = Bound Int
    | Free Atom
    deriving (Show, Eq)
type Type a = Term a

data Term a
    = Var a
    | Apply (Term a) (Term a)
    | Lambda Text (Maybe (Type a)) (Term a)
    | Pi Text (Type a) (Type a)
    | Type
    | Record [Entry a]
    | Sigma [Entry a]
    | Proj (Term a) (Term a)
    | Ascribe (Term a) (Type a)
    | Bool Bool
    | BoolT
    | If (Term a) (Term a) (Term a)
    | Int Int
    | IntT
    | Undefined
    deriving (Show, Eq)

data Entry a
    = Indexed (Term a)
    | Named Text (Term a)
    deriving (Show, Eq)

type Named = Term Text
type Nameless = Term Local

type Env a = StateT Int (Reader a)

strip :: Term a -> Term a
strip (Ascribe e _) = e
strip e = e

aeq :: Eq a => Term a -> Term a -> Bool
aeq a b =
    let a' = strip a
        b' = strip b
    in a' == b' || case (a', b') of
        (Lambda _ (Just t1) b1, Lambda _ (Just t2) b2) -> aeq t1 t2 && aeq b1 b2
        (Lambda _ _ b1, Lambda _ _ b2) -> aeq b1 b2
        (Pi _ t1 b1, Pi _ t2 b2) -> aeq t1 t2 && aeq b1 b2
        (Apply l1 r1, Apply l2 r2) -> aeq l1 l2 && aeq r1 r2
        (Proj e1 l1, Proj e2 l2) -> aeq e1 e2 && aeq l1 l2
        (Record f1, Record f2) -> all (uncurry entryAeq) (zip f1 f2)
        (Sigma f1, Sigma f2) -> all (uncurry entryAeq) (zip f1 f2)
        _ -> False
    where
        entryAeq (Indexed e1) (Indexed e2) = aeq e1 e2
        entryAeq (Named n1 e1) (Named n2 e2) = n1 == n2 && aeq e1 e2
        entryAeq _ _ = False

--- | open replaces the 0th bound variable in the second argument with the first
open :: Term Local -> Term Local -> Term Local
open a = go 0
    where
        go lvl = \case
            Var (Bound i) | i == lvl -> a
            Apply l r -> Apply (go lvl l) (go lvl r)
            Ascribe e t -> Ascribe (go lvl e) (go lvl t)
            Proj e l -> Proj (go lvl e) (go lvl l)
            Lambda n (Just t) b -> Lambda n (Just $ go lvl t) (go (lvl+1) b)
            Lambda n _ b -> Lambda n Nothing (go (lvl+1) b)
            Pi n t b -> Pi n (go lvl t) (go (lvl+1) b)
            Record es -> Record $ deepen lvl es
            Sigma es -> Sigma $ deepen lvl es
            e -> e
        deepen _ [] = []
        deepen lvl (Indexed x: xs) =
            Indexed (go lvl x):deepen lvl xs
        deepen lvl (Named n x:xs) =
            Named n (go (lvl+1) x): deepen (lvl+1) xs

bind :: Atom -> Term Local -> Term Local
bind a = go 0
    where 
        go lvl = \case
            Var (Free b) | a == b -> Var (Bound lvl)
            Apply l r -> Apply (go lvl l) (go lvl r)
            Ascribe e t -> Ascribe (go lvl e) (go lvl t)
            Proj e l -> Proj (go lvl e) (go lvl l)
            Lambda n (Just t) b -> Lambda n (Just $ go lvl t) (go (lvl+1) b)
            Lambda n _ b -> Lambda n Nothing (go (lvl+1) b)
            Pi n t b -> Pi n (go lvl t) (go (lvl+1) b)
            Record es -> Record $ deepen lvl es
            Sigma es -> Sigma $ deepen lvl es
            e -> e
        deepen _ [] = []
        deepen lvl (Indexed x: xs) =
            Indexed (go lvl x):deepen lvl xs
        deepen lvl (Named n x:xs) =
            Named n (go (lvl+1) x): deepen (lvl+1) xs

match :: MExpr -> Either Text Named
match (Atom "Type") = pure Type
match (Atom "False") = pure $ Bool False
match (Atom "True") = pure $ Bool True
match (Atom "Bool") = pure BoolT
match (Atom "Int") = pure IntT
match (Atom "Unit") = pure $ Sigma []
match (Atom a) = pure $ Var a
match (Operator o) = pure $ Var o
match (String _) = Left "unimplemented"
match (Number n) =
    case toBoundedInteger n of
        Just i -> Right $ Int i
        Nothing -> Left "not an integer"
match (Compound []) = pure $ Record []
match (Compound [Atom "if", a, b, c]) = do
    a' <- match a
    b' <- match b
    c' <- match c
    pure $ If a' b' c'
match (Compound (Atom "type":es)) = matchType (Compound es)
match (Compound es) =
    case findOperator [] es of
        Left _ -> traverse match es <&> foldl1 Apply
        Right (a, ":", b) -> do
            a' <- match $ Compound a
            b' <- matchType $ Compound b
            pure $ Ascribe a' b'
        Right (a, ".", b) -> do
            a' <- match $ Compound a
            b' <- match $ Compound b
            pure $ Proj a' b'
        Right (a, "->", b) -> do
            b' <- match $ Compound b
            foldrM lambda b' a
        Right (a, o, b) -> do
            a' <- match $ Compound a
            b' <- match $ Compound b
            pure $ Apply (Apply (Var o) a') b'
    where
        lambda a b = do
            a' <- match a
            case a' of
                Ascribe (Var x) t -> pure $ Lambda x (Just t) b
                Var x -> pure $ Lambda x Nothing b
                x -> Left $ "patterns unsupported, must bind a variable or an annotated variable\n" <> pack (show x)

match (Block es) = Record <$> traverse cons es
    where
        cons expr =
            case expr of
                Compound (Atom "let": Atom x: Operator ":": bnds) -> do
                    let (t,rst) = span (/= Operator "=") bnds
                    case rst of
                        (Operator "=":e) -> do
                            t' <- matchType $ Compound t
                            e' <- match (Compound e)
                            pure $ Named x (Ascribe e' t')
                        _ -> Left "didn't find equals in let"
                Compound (Atom "let": Atom x: Operator "=": e) -> do
                    e' <- match (Compound e)
                    pure $ Named x e'
                Compound (Atom x: Operator "=": e)  -> do
                    e' <- match $ Compound e
                    pure $ Named x e'
                _ -> do
                    e' <- match expr
                    pure $ Indexed e'

findOperator :: [MExpr] -> [MExpr] -> Either [MExpr] ([MExpr], Text, [MExpr])
findOperator x [] = Left x
findOperator x (Operator o: ys) = Right (x, o, ys)
findOperator x (y:ys) = findOperator (x ++ [y]) ys

matchType :: MExpr -> Either Text Named
matchType (Atom "Type") = pure Type
matchType (Atom "False") = pure $ Bool False
matchType (Atom "True") = pure $ Bool True
matchType (Atom "Bool") = pure BoolT
matchType (Atom "Int") = pure IntT
matchType (Atom "Unit") = pure $ Sigma []
matchType (Atom a) = pure $ Var a
matchType (Operator o) = pure $ Var o
matchType (String _) = Left "unimplemented"
matchType (Number n) =
    case toBoundedInteger n of
        Just i -> Right $ Int i
        Nothing -> Left "not an integer"
matchType (Compound []) = pure $ Sigma []
matchType (Compound [Atom "if", a, b, c]) = do
    a' <- match a
    b' <- match b
    c' <- match c
    pure $ If a' b' c'
matchType (Compound es) =
    case findOperator [] es of
        Left _ -> traverse matchType es <&> foldl1 Apply
        Right (a, ":", b) -> do
            a' <- matchType $ Compound a
            b' <- matchType $ Compound b
            pure $ Ascribe a' b'
        Right (a, ".", b) -> do
            a' <- matchType $ Compound a
            b' <- match $ Compound b
            pure $ Proj a' b'
        Right (a, "->", b) -> do
            b' <- matchType $ Compound b
            foldrM piType b' a
        Right (a, o, b) -> do
            a' <- matchType $ Compound a
            b' <- matchType $ Compound b
            pure $ Apply (Apply (Var o) a') b'
    where
        piType a u = do
            a' <- matchType a
            case a' of
                Ascribe (Var x) t -> pure $ Pi x t u
                t -> do
                    pure $ Pi "_" t u
matchType (Block es) = Sigma <$> traverse cons es
    where
        cons expr =
            case expr of
                Compound (Atom "let": Atom x: Operator ":": e) -> do
                    e' <- matchType (Compound e)
                    pure $ Named x e'
                Compound (Atom x: Operator ":": e)  -> do
                    e' <- matchType $ Compound e
                    pure $ Named x e'
                _ -> do
                    e' <- matchType expr
                    pure $ Indexed e'

toMExpr :: Named -> MExpr
toMExpr Type = Atom "Type"
toMExpr BoolT = Atom "Bool"
toMExpr IntT = Atom "Int"
toMExpr (Var n) = Atom n
toMExpr (Bool b) = Atom (pack (show b))
toMExpr (Int i) = Number (fromIntegral i)
toMExpr (Apply l r) =
    let lm = toMExpr l
        rm = toMExpr r
    in Compound [lm, rm]
toMExpr (Lambda n (Just t) b) =
    let texpr = toMExpr t
        bexpr = toMExpr b
    in Compound
            [ Compound [ Atom n, Operator ":", texpr ]
            , Operator "->"
            , bexpr
            ]
toMExpr (Lambda n _ b) =
    let bexpr = toMExpr b
    in Compound
            [ Atom n
            , Operator "->"
            , bexpr
            ]
toMExpr (Pi n t u) =
    let texpr = toMExpr t
        uexpr = toMExpr u
        p = if n == "_" then texpr else Compound [Atom n, Operator ":", texpr]
    in Compound
            [ p
            , Operator "->"
            , uexpr
            ]
toMExpr (If a b c) = Compound [Atom "if", toMExpr a, toMExpr b, toMExpr c]
toMExpr (Record tele) = Block $ map (build "=") tele
toMExpr (Sigma tele) = Block $ map (build ":") tele
toMExpr (Ascribe e t) =
    let e' = toMExpr e
        t' = toMExpr t
    in Compound [e', Operator ":", t']
toMExpr (Proj t l) =
    let e = toMExpr t
        l' = toMExpr l
    in Compound [e, Operator ".", l']
toMExpr Undefined = Atom "void"

build :: Text -> Entry Text -> MExpr
build _ (Indexed e) = toMExpr e
build _ (Named n (Ascribe e t)) =
    let e' = toMExpr e
        t' = toMExpr t
    in Compound [Atom "let", Atom n, Operator ":", t', Operator "=", e']
build o (Named n e) =
    let e' = toMExpr e
    in Compound [Atom n, Operator o, e']

prettySyntax :: Named -> String
prettySyntax = pretty . toMExpr