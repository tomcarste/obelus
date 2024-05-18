{-# LANGUAGE LambdaCase #-}
module Eval where
import Context
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound
normalize :: Bool -> Context -> Term -> Term
normalize cbv ctx = \case
        Var x -> maybe (Var x) (normalize cbv ctx) $ lookupDef ctx x
        Apply l r ->
            let e1 = normalize cbv ctx l in
            let e2 = if cbv then normalize cbv ctx r else r in

            case e1 of
              Lambda _ bound ->
                let b' = Unbound.instantiate bound [e2] in
                normalize cbv ctx b'
              _ -> Apply e1 e2
        e -> e