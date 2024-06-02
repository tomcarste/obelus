{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Typecheck where

import Context
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Except
import Eval (normalize)

type TcMonad = Unbound.FreshMT (ExceptT String IO)
infer :: Context -> Term -> TcMonad Type
infer ctx e =
    case e of
        Var x ->
            maybe (throwError ("not found " ++ show x)) pure $ lookupId ctx x
        Type -> pure Type
        Apply l r -> do
            ty1 <- infer ctx l
            ty <- normalize False ctx ty1 
            case ty of
                Pi t u -> do
                    check ctx r t
                    pure $ Unbound.instantiate u [r]
                _ -> throwError "expected function"
        Lambda (Just t) b -> do
            check ctx t Type
            (x,b') <- Unbound.unbind b
            u <- infer (addId ctx x t) b'
            pure (Pi t (Unbound.bind x u))
        Lambda _ _ -> throwError "Cannot infer type of lambda without argument type"
        Pi t u -> do
            check ctx t Type
            (x,u') <- Unbound.unbind u
            check (addId ctx x t) u' Type
            pure Type
        Sigma t -> do
            checkTele ctx t
            pure Type
        Record t -> do
            ts <- inferTele ctx t
            pure $ Sigma ts
        Proj e l -> do
            t <- infer ctx e
            l' <- normalize False ctx l
            
            case (t, l') of
                (Sigma r, Var x) -> do
                    let record = unfold 0 r
                    case lookup x record of
                        Just v -> pure v
                        Nothing -> throwError "field not found in record"
                (_, Var _) -> throwError "Attempted projection to something not a record"
                _ -> throwError "Projection must be to a label"
        Ascribe expr t -> do
            check ctx t Type
            check ctx expr t
            pure t
    where
        unfold :: Int -> Telescope -> [(Name, Term)]
        unfold _ [] = []
        unfold i (x:xs) =
            case x of
                Indexed e ->
                    let end = unfold (i+1) xs
                    in (Unbound.string2Name $ show i,  e):end
                Named n e ->
                    let end = unfold (i+1) xs
                    in (n, e): end
check :: Context -> Term -> Type -> TcMonad ()
check ctx e ty =
    case (e, ty) of
        (Lambda Nothing bndl, Pi t bndp) -> do
            (x, b, _, u) <- maybe (throwError "mismatched binding") pure =<< Unbound.unbind2 bndl bndp
            check (Context.addDef ctx x t) b u
        (Lambda Nothing _, _) -> throwError "expected function type"
        _ -> do
            t1 <- infer ctx e
            if Unbound.aeq t1 ty
                then pure ()
                else throwError ("type mismatch: " ++ prettyPrint t1 ++ " is not " ++ prettyPrint ty)

inferTele :: Context -> [Entry] -> TcMonad [Entry]
inferTele _ [] = pure []
inferTele ctx (x:xs) = 
    case x of
        Indexed e -> do
            t <- infer ctx e
            ts <- inferTele ctx xs
            pure (Indexed t:ts)
        Named n e -> do
            t <- infer ctx e
            ts <- inferTele (Context.addId ctx n t) xs
            pure (Named n t:ts)

checkTele :: Context -> [Entry] -> TcMonad ()
checkTele _ [] = pure ()
checkTele ctx (x:xs) = 
    case x of
        Indexed e -> do
            checkTele ctx xs
            check ctx e Type
        Named n e -> do
            checkTele (Context.addId ctx n e) xs
            check ctx e Type