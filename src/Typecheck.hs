{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Typecheck where

import Context
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Except
import Eval (normalize)
import Control.Monad.Reader

type TcMonad = Gamma
infer :: Term -> TcMonad Type
infer e =
    case e of
        Var x ->
            maybe (throwError ("not found: " ++ show x)) pure =<< asks (lookupId x)
        Type -> pure Type
        Apply l r -> do
            ty1 <- infer l
            ty <- normalize False ty1
            case ty of
                Pi t u -> do
                    check r t
                    pure $ Unbound.instantiate u [r]
                _ -> throwError $ "expected function got: " ++ show ty
        Lambda (Just t) b -> do
            check t Type
            (x, b') <- Unbound.unbind b
            u <- local (addId x t) $ infer b'
            pure (Pi t (Unbound.bind x u))
        Lambda _ _ -> throwError "Cannot infer type of lambda without argument type"
        Pi t u -> do
            check t Type
            (x,u') <- Unbound.unbind u
            local  (addId x t) $ check u' Type
            pure Type
        Sigma t -> do
            (r,()) <- Unbound.unbind t
            checkTele (toList r)
            pure Type
        Record t -> do
            (r,()) <- Unbound.unbind t
            ts <- inferTele (toList r)
            pure $ Sigma (Unbound.bind (fromList ts) ())
        Proj e l -> do
            t <- infer e
            l' <- normalize False l
            
            case (t, l') of
                (Sigma tele, Var x) -> do
                    (r, ()) <- Unbound.unbind tele
                    let record = unfold 0 (toList r)
                    case lookup (Unbound.name2String x) record of
                        Just v -> pure v
                        Nothing -> throwError $ "field: " ++ show x ++ " not found in record\n" ++ show record
                (_, Var _) -> throwError "Attempted projection to something not a record"
                _ -> throwError "Projection must be to a label"
        Ascribe expr t -> do
            check t Type
            check expr t
            pure t
    where
        unfold :: Int -> [Entry] -> [(String, Term)]
        unfold _ [] = []
        unfold i (x:xs) =
            case x of
                Indexed (Unbound.Embed e) ->
                    let end = unfold (i+1) xs
                    in (show i,  e):end
                Named n (Unbound.Embed e) ->
                    let end = unfold (i+1) xs
                    in (Unbound.name2String n, e): end
check :: Term -> Type -> TcMonad ()
check e ty = do
    ty' <- normalize False ty
    case (e, ty') of
        (Lambda _ bndl, Pi t bndp) -> do
            (x, b, _, u) <- maybe (throwError "mismatched binding") pure =<< Unbound.unbind2 bndl bndp
            local (Context.addId x t) $ check b u
        (Lambda _ _, _) -> throwError $ "expected function type, got: " ++ show ty' ++ " against " ++ show e
        _ -> do
            t1 <- infer e
            if Unbound.aeq t1 ty'
                then pure ()
                else throwError ("type mismatch: " ++ show t1 ++ " is not " ++ show ty')

inferTele :: [Entry] -> TcMonad [Entry]
inferTele [] = pure []
inferTele (x:xs) = 
    case x of
        Indexed (Unbound.Embed e) -> do
            t <- infer e
            ts <- inferTele xs
            pure (Indexed (Unbound.Embed t):ts)
        Named n (Unbound.Embed e) -> do
            t <- infer e
            ts <- local (Context.addId n t) $ inferTele xs
            pure (Named n (Unbound.Embed t):ts)

checkTele :: [Entry] -> TcMonad ()
checkTele [] = pure ()
checkTele (x:xs) = 
    case x of
        Indexed (Unbound.Embed e) -> do
            checkTele xs
            check e Type
        Named n (Unbound.Embed e) -> do
            local (Context.addId n e) $ checkTele xs
            check e Type