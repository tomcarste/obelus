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
            maybe (throwError "not found") pure $ lookupId ctx x
        Type -> pure Type
        Let _ t b -> do
            check ctx t Type
            check ctx b t
            pure Void
        Seq [] -> pure Void
        Seq [e'] -> infer ctx e'
        Seq (x:rest) -> do
            check ctx x Void
            infer ctx (Seq rest)
        Void -> pure Type
        Apply l r -> do
            ty1 <- infer ctx l
            case normalize False ctx ty1 of
                Pi t u -> do
                    check ctx r t
                    pure $ Unbound.instantiate u [r]
                _ -> throwError "expected function"
        Lambda t b -> do
            check ctx t Type
            (x,b') <- Unbound.unbind b
            u <- infer (addId ctx x t) b'
            pure (Pi t (Unbound.bind x u))
        Pi t u -> do
            check ctx t Type
            (x,u') <- Unbound.unbind u
            check (addId ctx x t) u' Type
            pure Type

            
check :: Context -> Term -> Type -> TcMonad ()
check ctx e ty = do
    t1 <- infer ctx e
    if Unbound.aeq t1 ty 
        then pure () 
        else throwError ("type mismatch: " ++ prettyPrint t1 ++ " is not " ++ prettyPrint ty)