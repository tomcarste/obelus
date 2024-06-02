{-# LANGUAGE LambdaCase #-}
module Eval where
import Context
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Reader

normalize :: Bool -> Term -> Gamma Term
normalize cbv = \case
  Var x ->
    maybe (pure $ Var x) (normalize cbv) =<< asks (lookupDef x)
  Apply l r -> do
      e1 <- normalize cbv l
      e2 <- if cbv then normalize cbv r else pure r

      case e1 of
        Lambda _ bound ->
          let b' = Unbound.instantiate bound [e2] in
          normalize cbv b'
        _ -> pure $ Apply e1 e2
  Proj a b -> do
    b' <- normalize cbv b
    a' <- normalize cbv a
    let deflt = Proj a' b'
    case (a',b') of
      (Record r, Var x) -> do
        (tele, ()) <- Unbound.unbind r
        normalizeTele deflt tele x
      (Sigma r, Var x) -> do
        (tele, ()) <- Unbound.unbind r
        normalizeTele deflt tele x
      _ -> pure deflt
  Ascribe e _ -> normalize cbv e
  e -> pure e
  where
    normalizeTele deflt r x = do
      ctx <- ask
      let unfolded = unfold ctx 0 (toList r)
      record <- if cbv then traverse normalizeFold unfolded else pure unfolded
      case lookup x record of
        Just (ctx', v) -> if cbv then pure v else local (const ctx') $ normalize cbv v
        Nothing -> pure deflt

    normalizeFold (n, (ctx', e)) = do
      e' <- local (const ctx') $ normalize cbv e
      pure (n, (ctx', e'))
    unfold :: Context -> Int -> [Entry] -> [(Name, (Context, Term))]
    unfold _ _ [] = []
    unfold ctx0 i (x:xs) =
      case x of
        Indexed (Unbound.Embed e) ->
          let end = unfold ctx0 (i+1) xs
          in (Unbound.string2Name $ show i, (ctx0, e)):end
        Named n (Unbound.Embed e) ->
          let end = unfold (Context.addDef n e ctx0) (i+1) xs
          in (n, (ctx0, e)): end
