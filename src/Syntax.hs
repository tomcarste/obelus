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
import Data.Foldable (foldrM)

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
match' (Compound (Atom "type":es)) = matchType (Compound es)
match' (Compound es) = 
    case findOperator [] es of
        Left _ -> traverse match' es <&> foldl1 Apply
        Right (a, ":", b) -> do
            a' <- match' $ Compound a
            b' <- matchType $ Compound b
            pure $ Ascribe a' b'
        Right (a, ".", b) -> do
            a' <- match' $ Compound a
            b' <- match' $ Compound b
            pure $ Proj a' b'
        Right (a, "->", b) -> do
            b' <- match' $ Compound b
            foldrM lambda b' a
        Right (a, o, b) -> do
            a' <- match' $ Compound a
            b' <- match' $ Compound b
            let o' = Unbound.string2Name (unpack o)
            pure $ Apply (Apply (Var o') a') b'
    where
        lambda a b = do
            a' <- match' a
            case a' of
                Ascribe (Var x) t -> pure $ Lambda (Just t) (Unbound.bind x b)
                Var x -> pure $ Lambda Nothing (Unbound.bind x b)
                x -> throwError $ "patterns unsupported, must bind a variable or an annotated variable\n" ++ show x

match' (Block es) = Record . flip Unbound.bind () . fromList <$> traverse cons es
    where
        cons expr =
            case expr of
                Compound (Atom "let": Atom x: Operator ":": bnds) -> do
                    let (t,rst) = span (/= Operator "=") bnds
                    case rst of
                        (Operator "=":e) -> do
                            t' <- matchType $ Compound t
                            e' <- match' (Compound e)
                            pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed $ Ascribe e' t')
                        _ -> throwError "didn't find equals in let"
                Compound (Atom "let": Atom x: Operator "=": e) -> do
                    e' <- match' (Compound e)
                    pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed e')
                Compound (Atom x: Operator "=": e)  -> do
                    e' <- match' $ Compound e
                    pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed e')
                _ -> do
                    e' <- match' expr
                    pure $ Indexed (Unbound.Embed e')

findOperator :: [MExpr] -> [MExpr] -> Either [MExpr] ([MExpr], Text, [MExpr])
findOperator x [] = Left x
findOperator x (Operator o: ys) = Right (x, o, ys)
findOperator x (y:ys) = findOperator (x ++ [y]) ys

matchType :: MExpr -> Unbound.FreshMT (Except String) Term
matchType (Atom "Type") = pure Type
matchType (Atom "Unit") = pure $ Sigma (Unbound.bind Nil ())
matchType (Atom a) = pure . Var . Unbound.string2Name $ unpack a
matchType (Operator o) = pure . Var . Unbound.string2Name $ unpack o
matchType (String _) = throwError "unimplemented"
matchType (Number _) = throwError "unimplemented"
matchType (Compound []) = pure $ Sigma (Unbound.bind Nil ())
matchType (Compound es) = 
    case findOperator [] es of
        Left _ -> traverse matchType es <&> foldl1 Apply
        Right (a, ":", b) -> do
            a' <- matchType $ Compound a
            b' <- matchType $ Compound b
            pure $ Ascribe a' b'
        Right (a, ".", b) -> do
            a' <- matchType $ Compound a
            b' <- match' $ Compound b
            pure $ Proj a' b'
        Right (a, "->", b) -> do
            b' <- matchType $ Compound b
            foldrM piType b' a
        Right (a, o, b) -> do
            a' <- matchType $ Compound a
            b' <- matchType $ Compound b
            let o' = Unbound.string2Name (unpack o)
            pure $ Apply (Apply (Var o') a') b'
    where
        piType a b = do
            a' <- matchType a
            case a' of
                Ascribe (Var x) t -> pure $ Pi t (Unbound.bind x b)
                t -> do
                    x <- Unbound.fresh (Unbound.string2Name "_")
                    pure $ Pi t (Unbound.bind x b)
matchType (Block es) = Sigma . flip Unbound.bind () . fromList <$> traverse cons es
    where
        cons expr =
            case expr of
                Compound (Atom "let": Atom x: Operator ":": e) -> do
                    e' <- matchType (Compound e)
                    pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed e')
                Compound (Atom x: Operator ":": e)  -> do
                    e' <- matchType $ Compound e
                    pure $ Named (Unbound.string2Name $ unpack x) (Unbound.Embed e')
                _ -> do
                    e' <- matchType expr
                    pure $ Indexed (Unbound.Embed e')

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