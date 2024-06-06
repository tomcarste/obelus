module Repl where

import Syntax
import Desugar
import Context
import Eval
import Typecheck
import MlExpr
import qualified Data.Text as T
import System.Console.Haskeline
import Control.Monad.Reader

repl :: IO ()
repl = void $ runGamma go
    where
        go :: Gamma ()
        go = do
            l <- runInputT defaultSettings $ getInputLine "%"
            case l of
                Nothing -> go
                Just s ->
                    case parse (T.pack s) >>= match  of
                        Right es -> do
                            bnds <- toNameless es
                            ctx' <- eval bnds
                            local (const ctx') go
                            
                        Left err -> do
                            liftIO $ print err
                            go
        eval e = do
            ctx <- ask
            (e1, t) <- infer e
            e' <- normalize True e1
            case (t, e') of
                (Sigma rt, v@(Record r)) -> do
                    liftIO $ putStrLn ("signature : " ++ show t ++ "\nmodule = " ++ show v)
                    foldM addRecord2 ctx (zip rt r)
                (_, v) -> do
                    liftIO $ putStrLn ("type : " ++ show t ++ "\nvalue = " ++ show v)
                    pure ctx


addRecord2 :: Context -> (Entry Local, Entry Local) -> Gamma Context
addRecord2 ctx (Named n e, Named _  t) = do
    n' <- fresh n
    pure $ Context.addId n' t (Context.addDef n' e ctx)
addRecord2 ctx _ = pure ctx