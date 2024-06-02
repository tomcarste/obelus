{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Syntax (Name, Type, Term(..), Entry(..), Telescope, match, toMExpr, prettyPrint, toList, fromList) where
import Data.Function (on)
import Data.Text (unpack, pack, Text)
import GHC.Generics (Generic, from)
import qualified Unbound.Generics.LocallyNameless as Unbound
import MlExpr
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Functor ((<&>))
import GHC.Stack (HasCallStack)

type Name = Unbound.Name Term
type Type = Term
data Term
    = Var Name
    | Apply Term Term
    | Lambda (Maybe Type) (Unbound.Bind Name Term)
    | Pi Type (Unbound.Bind Name Type)
    | Type
    | Record (Unbound.Bind (Telescope Entry) ())
    | Sigma (Unbound.Bind (Telescope Entry) ())
    | Proj Term Term
    | Ascribe Term Type
    deriving (Show, Generic)

instance Unbound.Subst Term Term where
    isvar (Var x) = Just (Unbound.SubstName x)
    isvar _ = Nothing
strip :: Term -> Term
strip (Ascribe e _) = e
strip e = e
instance Unbound.Alpha Term where
    aeq' :: Unbound.AlphaCtx -> Term -> Term -> Bool
    aeq' ctx a b= (Unbound.gaeq ctx `on` from) (strip a) (strip b)

instance Eq Term where
    (==) = Unbound.aeq

data Entry 
    = Indexed (Unbound.Embed Term)
    | Named Name (Unbound.Embed Term)
    deriving (Show, Generic, Unbound.Alpha, Unbound.Subst Term)

data Telescope a
    = Nil
    | Cons (Unbound.Rebind a (Telescope a))
    deriving (Show, Generic, Unbound.Alpha)
instance Unbound.Subst Term (Telescope Entry)

toList :: Unbound.Alpha a => Telescope a -> [a]
toList Nil = []
toList (Cons bnd) =
    let (a, rst) = Unbound.unrebind bnd
    in a : toList rst
fromList :: [Entry] -> Telescope Entry
fromList = foldr (\ a b -> Cons (Unbound.rebind a b)) Nil
match :: MExpr -> Either String Term
match e =
    runExcept . Unbound.runFreshMT $ match' e
match' :: MExpr -> Unbound.FreshMT (Except String) Term
match' (Atom "Type") = pure Type
match' (Atom "Unit") = pure $ Sigma (Unbound.bind Nil ())
match' (Atom a) = pure . Var . Unbound.string2Name $ unpack a
match' (Operator o) = pure . Var . Unbound.string2Name $ unpack o
match' (String _) = throwError "unimplemented"
match' (Number _) = throwError "unimplemented"
match' (Compound []) = pure $ Record (Unbound.bind Nil ())
match' (Compound [Atom "type", e]) = matchType e
match' (Compound [e, Operator ".", l]) = do
    e' <- match' e
    l' <- match' l
    pure $ Proj e' l'
match' (Compound (Compound (Atom a: Operator ":": t):Operator "->": b)) = do
    ttype <- matchType $ Compound t
    bterm <- match' (Compound b)
    pure $ Lambda (Just ttype) (Unbound.bind (Unbound.string2Name $ unpack a) bterm)
match' (Compound (Atom a: Operator "->": b)) = do
    bterm <- match' (Compound b)
    pure $ Lambda Nothing (Unbound.bind (Unbound.string2Name $ unpack a) bterm)
match' (Compound es) = traverse match' es <&> foldl1 Apply
match' (Block es) = Record . flip Unbound.bind () . fromList <$> traverse (cons "=") es


cons :: Text -> MExpr -> Unbound.FreshMT (Except String) Entry
cons o expr =
    case expr of
        Compound (Atom "let": Atom x: Operator ":": bnds) -> do
            let (t,rst) = span (/= Operator "=") bnds
            case rst of
                (Operator "=":e) -> do
                    t' <- matchType $ Compound t
                    e' <- match' (Compound e)
                    pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed $ Ascribe e' t')
                _ -> throwError "didn't find equals in let"
        Compound (Atom "let": Atom x: Operator o': e) | o == o' -> do
            e' <- match' (Compound e)
            pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed e')
        Compound (Atom x: Operator o': e)  | o == o' -> do
            e' <- match' $ Compound e
            pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed e')
        _ -> do
            e' <- match' expr
            pure $ Indexed (Unbound.Embed e')
matchType :: MExpr -> Unbound.FreshMT (Except String) Term
matchType (Atom "Type") = pure Type
matchType (Atom "Unit") = pure $ Sigma (Unbound.bind Nil ())
matchType (Atom a) = pure . Var . Unbound.string2Name $ unpack a
matchType (Operator o) = pure . Var . Unbound.string2Name $ unpack o
matchType (String _) = throwError "unimplemented"
matchType (Number _) = throwError "unimplemented"
matchType (Compound []) = pure $ Sigma (Unbound.bind Nil ())
matchType (Compound [e, Operator ".", l]) = do
    e' <- matchType e
    l' <- match' l
    pure $ Proj e' l'
matchType (Compound (Compound (Atom a: Operator ":": t):Operator "->": u)) = do
    ttype <- matchType $ Compound t
    utype <- matchType $ Compound u
    pure $ Pi ttype (Unbound.bind (Unbound.string2Name $ unpack a) utype)
matchType (Compound (e:Operator "->": u)) = do
    name <- match' e
    fresh <- Unbound.fresh (Unbound.string2Name "_")
    utype <- matchType $ Compound u
    pure $ Pi name (Unbound.bind fresh utype)
matchType (Compound es) = traverse matchType es <&> foldl1 Apply
matchType (Block es) = Sigma . flip Unbound.bind () . fromList <$> traverse (cons ":") es

toMExpr :: (HasCallStack, Unbound.Fresh m) => Term -> m MExpr
toMExpr Type = pure $ Atom "Type"
toMExpr (Var n) = pure . Atom . pack $ show n
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
    let n = Unbound.name2String x
    let p = if n == "_" then texpr else Compound [Atom $ pack n, Operator ":", texpr]
    pure $ Compound
            [ p
            , Operator "->"
            , uexpr
            ]
toMExpr (Record tele) = do
    (r,()) <- Unbound.unbind tele
    Block <$> traverse (build "=") (toList r)
toMExpr (Sigma tele) = do
    (r,()) <- Unbound.unbind tele
    Block <$> traverse (build ":") (toList r)
toMExpr (Ascribe e t) = do
    e' <- toMExpr e
    t' <- toMExpr t
    pure $ Compound [e', Operator ":", t']
toMExpr (Proj t l) = do
    e <- toMExpr t
    l' <- toMExpr l
    pure $ Compound [e, Operator ".", l']

build :: Unbound.Fresh m => Text -> Entry -> m MExpr
build _ (Indexed (Unbound.Embed e)) = toMExpr e
build _ (Named n (Unbound.Embed (Ascribe e t))) = do
    e' <- toMExpr e
    t' <- toMExpr t
    pure $ Compound [Atom "let", Atom . pack $ Unbound.name2String n, Operator ":", t', Operator "=", e']
build o (Named n (Unbound.Embed e)) = do
    e' <- toMExpr e
    pure $ Compound [Atom . pack $ Unbound.name2String n, Operator o, e']

prettyPrint :: HasCallStack => Term -> String
prettyPrint = pretty . Unbound.runFreshM . toMExpr