{-# LANGUAGE LambdaCase #-}
module Eval where
import Context
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound

normalize :: Unbound.Fresh m => Bool -> Context -> Term -> m Term
normalize cbv ctx = \case
  Var x -> maybe (pure $ Var x) (normalize cbv ctx) $ lookupDef ctx x
  Apply l r -> do
      e1 <- normalize cbv ctx l
      e2 <- if cbv then normalize cbv ctx r else pure r

      case e1 of
        Lambda _ bound ->
          let b' = Unbound.instantiate bound [e2] in
          normalize cbv ctx b'
        _ -> pure $ Apply e1 e2
  Proj a b -> do
    b' <- normalize cbv ctx b
    a' <- normalize cbv ctx a
    let deflt = Proj a' b'
    case (a',b') of
      (Record r, Var x) -> normalizeTele deflt r x
      (Sigma r, Var x) -> normalizeTele deflt r x
      _ -> pure deflt
  Ascribe e _ -> normalize cbv ctx e
  e -> pure e
  where
    normalizeTele deflt r x = do
      let unfolded = unfold ctx 0 r
      record <- if cbv then traverse normalizeFold unfolded else pure unfolded
      case lookup x record of
        Just (ctx', v) -> if cbv then pure v else normalize cbv ctx' v
        Nothing -> pure deflt
    
    normalizeFold (n, (ctx', e)) = do 
      e' <- normalize cbv ctx' e
      pure (n, (ctx', e'))
    unfold :: Context -> Int -> Telescope -> [(Name, (Context, Term))]
    unfold _ _ [] = []
    unfold ctx0 i (x:xs) =
      case x of
        Indexed e ->
          let end = unfold ctx0 (i+1) xs
          in (Unbound.string2Name $ show i, (ctx0, e)):end
        Named n e ->
          let end = unfold (Context.addDef ctx0 n e) (i+1) xs
          in (n, (ctx0, e)): end
