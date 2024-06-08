{-# LANGUAGE LambdaCase #-}
module Eval where
import Context
import Syntax
import Control.Monad.Reader

normalize :: Bool -> Nameless -> Gamma Nameless
normalize cbv = \case
  Var (Free x) -> do
    v <- asks (lookupDef x)
    maybe (pure $ Var (Free x)) (normalize cbv) v
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
      
      (Record r, Int i) -> do
        res <- reach i r
        case res of
          Just v -> pure v
          Nothing -> pure deflt
      (Sigma r, Int i) -> do
        res <- reach i r
        case res of
          Just v -> pure v
          Nothing -> pure deflt
      _ -> pure deflt
  Ascribe e _ -> normalize cbv e
  Record r ->
    if cbv then Record <$> normEntries r else pure (Record r)
  If a b c -> do
    a' <- normalize cbv a
    case a' of
      Bool True -> normalize cbv b
      Bool False -> normalize cbv c
      _ -> pure $ If a' b c
  e -> pure e
  where
    search _ [] = pure Nothing
    search x (Indexed _: xs) = search x xs
    search x (Named n e: xs) =
      if x == n
        then do
          e' <- normalize cbv e
          pure $ Just e'
        else do
          n' <- fresh n
          e' <-  normalize cbv e
          let (Record r) = open (Var (Free n')) (Record xs)
          local (Context.addDef n' e') $ search x r
    search x (LetR n e _: xs) =
      if x == n
        then do
          n' <- fresh n
          let x' = open (Var (Free n')) e
          e' <- local (Context.addDef n' x') $ normalize cbv x'
          pure $ Just e'
        else do
          n' <- fresh n
          e' <- local (Context.addDef n' e) $ normalize cbv (open (Var (Free n')) e)
          let (Record r) = open (Var (Free n')) (Record xs)
          local (Context.addDef n' e') $ search x r
    reach _ [] = pure Nothing
    reach 0 (Indexed e: _) = pure $ Just e
    reach x (Indexed _: xs) = reach (x-1) xs
    reach 0 (Named _ e: _) = do
          e' <-  normalize cbv e
          pure $ Just e'
    reach x (Named n e: xs) = do
          n' <- fresh n
          e' <- normalize cbv e
          let (Record r) = open (Var (Free n')) (Record xs)
          local (Context.addDef n' e') $ reach (x-1) r
    reach 0 (LetR n e _: _) = do
          n' <- fresh n
          let x' = open (Var (Free n')) e
          e' <- local (Context.addDef n' x') $ normalize cbv x'
          pure $ Just e'
    reach x (LetR n e _: xs) = do
          n' <- fresh n
          e' <- local (Context.addDef n' e) $ normalize cbv (open (Var (Free n')) e)
          let (Record r) = open (Var (Free n')) (Record xs)
          local (Context.addDef n' e') $ reach (x-1) r
    normEntries [] = pure []
    normEntries (Indexed x:xs) = do
      x' <- normalize cbv x
      xs' <- normEntries xs
      pure (Indexed x':xs')
    normEntries (Named n x:xs) = do
      n' <- fresh n
      x'' <- normalize cbv x
      let (Record r) = open (Var (Free n')) (Record xs)
      xs' <- local (Context.addDef n' x'') $ normEntries r
      pure (Named n x'':xs')
    normEntries (LetR n x t:xs) = do
      n' <- fresh n
      let x' = open (Var (Free n')) x
      x'' <- local (Context.addDef n' x') $ normalize cbv x'
      let (Record r) = open (Var (Free n')) (Record xs)
      xs' <- local (Context.addDef n' x'') $ normEntries r
      pure (LetR n x'' t:xs')

-- used to reduce modules to whnf
evaluate = \case
  Record r -> Record <$> normEntries r
  e -> normalize False e
  where
    normEntries [] = pure []
    normEntries (Indexed x:xs) = do
      x' <- normalize False x
      xs' <- normEntries xs
      pure (Indexed x':xs')
    normEntries (Named n x:xs) = do
      n' <- fresh n
      x'' <- normalize False x
      let (Record r) = open (Var (Free n')) (Record xs)
      xs' <- local (Context.addDef n' x'') $ normEntries r
      pure (Named n x'':xs')
    normEntries (LetR n x t:xs) = do
      n' <- fresh n
      let x' = open (Var (Free n')) x
      x'' <- local (Context.addDef n' x') $ normalize False x'
      let (Record r) = open (Var (Free n')) (Record xs)
      xs' <- local (Context.addDef n' x'') $ normEntries r
      pure (LetR n x'' t:xs')