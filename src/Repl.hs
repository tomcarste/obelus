module Repl where

import Syntax
import Desugar
import Context
import Eval
import Typecheck
import MlExpr
import qualified Data.Text as T
import qualified Data.Map as Map
import System.Console.Haskeline
import Control.Monad.Reader
import Control.Monad.Except (MonadError(catchError))

repl :: IO ()
repl = void $ runGamma go
    where
        go :: Gamma ()
        go = do
            l <- runInputT defaultSettings $ getInputLine "%"
            case l of
                Nothing -> go
                Just ":q" -> pure ()
                Just s ->
                    case parse (T.pack s) >>= match  of
                        Right es -> do
                            catchError (do
                                ctx <- ask
                                bnds <- toNamelessWith (Map.fromList (zip (fmap (fst . fst) (defs ctx)) [0..])) es
                                ctx' <- eval bnds
                                local (const ctx') go
                                ) (\ e -> liftIO (print e) >> go)

                        Left err -> do
                            liftIO $ print err
                            go
        eval e = do
            ctx <- ask
            (_, t) <- infer e
            e' <- normalize True e
            t' <- toNamed t
            e1 <- toNamed e'
            case (t, e') of
                (Sigma rt, Record r) -> do

                    liftIO $ putStrLn ("signature : " ++ prettySyntax t' ++ "\nmodule = " ++ prettySyntax e1)
                    foldM addRecord2 ctx (zip rt r)
                _ -> do
                    liftIO $ putStrLn ("type : " ++ show t' ++ "\nvalue = " ++ show e1)
                    pure ctx


addRecord2 :: Context -> (Entry Local, Entry Local) -> Gamma Context
addRecord2 ctx (Named n e, Named _  t) = do
    n' <- fresh n
    pure $ Context.addId n' t (Context.addDef n' e ctx)
addRecord2 ctx _ = pure ctx