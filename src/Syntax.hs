{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Syntax (Name, Type, Term(..), Entry(..), Telescope, match, toMExpr, prettyPrint) where
import Data.Function (on)
import Data.Text (unpack, pack, Text)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound
import MlExpr
import Control.Monad (foldM)

type Name = Unbound.Name Term
type Type = Term
data Term
    = Var Name
    | Apply Term Term
    | Lambda (Maybe Type) (Unbound.Bind Name Term)
    | Pi Type (Unbound.Bind Name Type)
    | Type
    | Record Telescope
    | Sigma Telescope
    | Proj Term Term
    | Ascribe Term Type
    deriving (Show, Generic, Unbound.Subst Term)

strip :: Term -> Term
strip (Ascribe e _) = e
strip e = e
instance Unbound.Alpha Term where
    aeq' :: Unbound.AlphaCtx -> Term -> Term -> Bool
    aeq' ctx a b= (Unbound.gaeq ctx `on` from) (strip a) (strip b)
    
instance Eq Term where
    (==) = Unbound.aeq

data Entry 
    = Indexed Term
    | Named Name Term
    deriving (Show, Generic, Unbound.Alpha, Unbound.Subst Term)
type Telescope = [Entry]

match :: MExpr -> Either String Term
match (Atom "Type") = Right Type
match (Atom "Unit") = Right $ Sigma []
match (Atom a) = Right . Var . Unbound.string2Name $ unpack a
match (Operator o) = Right . Var . Unbound.string2Name $ unpack o
match (String _) = Left "unimplemented"
match (Number _) = Left "unimplemented"
match (Compound []) = Right $ Record []
match (Compound [Atom "type", e]) = matchType e
match (Compound [e, Operator ".", l]) = do
    e' <- match e
    l' <- match l
    pure $ Proj e' l'
match (Compound (Compound [Atom a, Operator ":", t]:Operator "->": b)) = do
    ttype <- matchType t
    bterm <- match (Compound b)
    Right $ Lambda (Just ttype) (Unbound.bind (Unbound.string2Name $ unpack a) bterm)
match (Compound (Atom a: Operator "->": b)) = do
    bterm <- match (Compound b)
    Right $ Lambda Nothing (Unbound.bind (Unbound.string2Name $ unpack a) bterm)
match (Compound es) = traverse match es >>= Right . foldl1 Apply
match (Block es) = Record <$> foldM (cons "=") [] es

cons :: Text -> Telescope -> MExpr -> Either String Telescope
cons o tele expr =
    case expr of
        Compound (Atom "let": Atom x: Operator ":": t: Operator "=": e) -> do
            t' <- matchType t
            e' <- match (Compound e)
            pure $ Named (Unbound.string2Name $ unpack x) (Ascribe e' t'):tele
        Compound (Atom "let": Atom x: Operator o': e) | o == o' -> do
            e' <- match (Compound e)
            pure $ Named (Unbound.string2Name $ unpack x)e': tele
        Compound (Atom x: Operator o': e)  | o == o' -> do
            e' <- match $ Compound e
            pure $ Named (Unbound.string2Name $ unpack x)e': tele
        _ -> do
            e' <- match expr
            pure $ Indexed e': tele

matchType :: MExpr -> Either String Type
matchType (Atom "Type") = Right Type
matchType (Atom "Unit") = Right $ Sigma  []
matchType (Atom a) = Right . Var . Unbound.string2Name $ unpack a
matchType (Operator o) = Right . Var . Unbound.string2Name $ unpack o
matchType (String _) = Left "unimplemented"
matchType (Number _) = Left "unimplemented"
matchType (Compound []) = Right $ Sigma []
matchType (Compound [e, Operator ".", l]) = do
    e' <- match e
    l' <- match l
    pure $ Proj e' l'
matchType (Compound (Compound [Atom a, Operator ":", t]:Operator "->": u)) = do
    ttype <- matchType t
    utype <- matchType $ Compound u
    Right $ Pi ttype (Unbound.bind (Unbound.string2Name $ unpack a) utype)
matchType (Compound es) = traverse matchType es >>= Right . foldl1 Apply
matchType (Block es) = Sigma <$> foldM (cons ":") [] es

toMExpr :: Unbound.Fresh m => Term -> m MExpr
toMExpr Type = pure $ Atom "Type"
toMExpr (Var n) = pure . Atom . pack $ Unbound.name2String n
toMExpr (Apply l r) = do
    lm <- toMExpr l
    rm <- toMExpr r
    pure $ Compound [lm, rm]
toMExpr (Lambda (Just t) a) = do
    (x,b) <- Unbound.unbind a
    texpr <- toMExpr t
    bexpr <- toMExpr b
    pure $ Compound 
            [ Compound [ Atom . pack $ Unbound.name2String x, Operator ":", texpr ]
            , Operator "->"
            , bexpr
            ]
toMExpr (Lambda _ a) = do
    (x,b) <- Unbound.unbind a
    bexpr <- toMExpr b
    pure $ Compound 
            [ Atom . pack $ Unbound.name2String x
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
toMExpr (Record tele) = do
    Block <$> unfold "=" tele
toMExpr (Sigma tele) = do
    Block <$> unfold ":" tele
toMExpr (Ascribe e t) = do
    e' <- toMExpr e
    t' <- toMExpr t
    pure $ Compound [e', Operator ":", t']
toMExpr (Proj t l) = do
    e <- toMExpr t
    l' <- toMExpr l
    pure $ Compound [e, Operator ".", l']
    
unfold :: Unbound.Fresh m => String -> Telescope -> m [MExpr]
unfold _ [] = pure []
unfold o (x:xs) =
    case x of
        Indexed e -> do
            rest <- unfold o xs
            frst <- toMExpr e
            pure (frst : rest)
        Named n (Ascribe e t) -> do
            let label = Unbound.name2String n 
            t' <- toMExpr t
            rest <- unfold o xs
            frst <- toMExpr e
            pure (Compound [Atom "let", Atom $ pack label, Operator ":", t', Operator "=", frst] : rest)
        Named n e -> do
            let label = Unbound.name2String n 
            rest <- unfold o xs
            frst <- toMExpr e
            pure (Compound [Atom $ pack label, Operator $ pack o, frst] : rest)
prettyPrint :: Term -> String
prettyPrint = pretty . Unbound.runFreshM . toMExpr 