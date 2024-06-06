{-# LANGUAGE LambdaCase #-}
module Eval where
import Context
import Syntax
import Control.Monad.Reader
import Debug.Trace (traceM)

normalize :: Bool -> Nameless -> Gamma Nameless
normalize cbv = \case
  Var (Free x) ->
    maybe (pure $ Var (Free x)) (normalize cbv) =<< asks (lookupDef x)
  Apply l r -> do
      e1 <- normalize cbv l
      e2 <- if cbv then normalize cbv r else pure r
      case e1 of
        Lambda _ _ b ->
          let b' = open e2 b in
          normalize cbv b'
        _ -> pure $ Apply e1 e2
  Proj a b -> do
    b' <- normalize cbv b
    a' <- normalize cbv a
    let deflt = Proj a' b'
    case (a',b') of
      (Record r, Var (Free (x,_))) -> do
        res <- search x r
        case res of
          Just v -> pure v
          Nothing -> pure deflt
      (Sigma r, Var (Free (x,_))) -> do
        res <- search x r
        case res of
          Just v -> pure v
          Nothing -> pure deflt
      _ -> pure deflt
  Ascribe e _ -> normalize cbv e
  Record r ->
    Record <$> normEntries r
  e -> pure e
  where
    search _ [] = pure Nothing
    search x (Indexed _: xs) = search x xs
    search x (Named n e: xs) =
      if x == n
        then pure $ Just e
        else do
          n' <- fresh n
          e' <- local (Context.addDef n' e) $ normalize cbv (open (Var (Free n')) e)
          local (Context.addDef n' e') $ search x xs
    normEntries [] = pure []
    normEntries (Indexed x:xs) = do
      x' <- normalize cbv x
      xs' <- normEntries xs
      pure (Indexed x':xs')
    normEntries (Named n x:xs) = do
      x' <- normalize cbv x
      n' <- fresh n
      xs' <- local (Context.addDef n' x') $ normEntries xs
      pure (Named n x':xs')
