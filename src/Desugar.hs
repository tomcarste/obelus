{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Desugar where

import qualified Data.Map as Map
import Data.Text
import Syntax
import Context
import Control.Monad.Except
toNameless :: Named -> Gamma Nameless
toNameless = go mempty
    where
        go :: Map.Map Text Int -> Named -> Gamma Nameless
        go env = \case
            Type -> pure Type
            Var a ->
                case Map.lookup a env of
                    Just bv -> pure $ Var (Bound bv)
                    Nothing -> Var . Free <$> fresh a
            Apply l r -> Apply <$> go env l <*> go env r
            Ascribe b t -> Ascribe <$> go env b <*> go env t
            Proj a l -> Proj <$> go env a <*> go env l
            Lambda n (Just t) b -> do
                t' <- go env t
                b' <- go (extend n env) b
                pure $ Lambda n (Just t') b'
            Lambda n _ b -> do
                b' <- go (extend n env) b
                pure $ Lambda n Nothing b'
            Pi n t u -> do
                t' <- go env t
                u' <- go (extend n env) u
                pure $ Pi n t' u'
            Record es -> Record <$> deepen env es
            Sigma es -> Sigma <$> deepen env es
        deepen :: Map.Map Text Int -> [Entry Text] -> Gamma [Entry Local]
        deepen _ [] = pure []
        deepen env (Indexed x:xs) = do
            x' <- go env x
            xs' <- deepen env xs
            pure $ Indexed x' : xs'
        deepen env (Named n x : xs) = do
            x' <- go env x
            xs' <- deepen (extend n env) xs
            pure $ Named n x' : xs'
        extend n = Map.insert n 0 . Map.map (+ 1)

toNamed :: Nameless -> Gamma Named
toNamed = go mempty
    where
        go env = \case
            Type -> pure Type
            Var (Bound bv) ->
                case Map.lookup bv env of
                    Just name -> pure $ Var name
                    Nothing -> throwError $ "bound variable without a binder: " <> pack (show bv)
            Var (Free (a,_)) -> pure $  Var a
            Apply l r -> Apply <$> go env l <*> go env r
            Ascribe b t -> Ascribe <$> go env b <*> go env t
            Proj a l -> Proj <$> go env a <*> go env l
            Lambda n (Just t) b -> do
                t' <- go env t
                b' <- go (extend n env) b
                pure $ Lambda n (Just t') b'
            Lambda n _ b -> do
                b' <- go (extend n env) b
                pure $ Lambda n Nothing b'
            Pi n t u -> do
                t' <- go env t
                u' <- go (extend n env) u
                pure $ Pi n t' u'
            Record es -> Record <$> deepen env es
            Sigma es -> Sigma <$> deepen env es
        deepen _ [] = pure []
        deepen env (Indexed x:xs) = do
            x' <- go env x
            xs' <- deepen env xs
            pure $ Indexed x' : xs'
        deepen env (Named n x : xs) = do
            x' <- go env x
            xs' <- deepen (extend n env) xs
            pure $ Named n x' : xs'
        extend n = Map.insert 0 n . Map.mapKeysMonotonic (+ 1)
