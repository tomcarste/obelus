module Repl where

import Syntax
import Context
import Eval
import Typecheck
import MlExpr
import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Data.Text as T
import Control.Monad.Except (runExceptT)
import System.Console.Haskeline
import Control.Monad (foldM)

repl :: IO ()
repl = go (Context {ids=[],defs=[]})
    where
        go :: Context -> IO ()
        go ctx =
            do
            l <- runInputT defaultSettings $ getInputLine "%"
            case l of
                Just s -> do
                    case parse (T.pack s) >>= match . Block  of
                        Right es -> do
                            ctx' <- eval ctx es
                            go ctx'
                        Left err -> do
                            print err
                            go ctx
                Nothing -> pure ()
        eval ctx e = do
            result <- runExceptT $ Unbound.runFreshMT $ infer ctx e
            case result of
                Left err -> do
                    print err
                    pure ctx
                Right t -> do
                    case (t, Unbound.runFreshM $ normalize True ctx e) of
                        (Sigma rt, v@(Record r)) -> do
                            putStrLn ("signature : " ++ prettyPrint t ++ "\nmodule = " ++ prettyPrint v)
                            foldM addRecord2 ctx (zip rt r)
                        (_, v) -> do
                            putStrLn ("type : " ++ prettyPrint t ++ "\nvalue = " ++ prettyPrint v)
                            pure ctx

addRecord2 :: Context -> (Entry, Entry) -> IO Context
addRecord2 ctx (Named n t, Named _ e) = do
    pure $ Context.addId (Context.addDef ctx n e) n t
addRecord2 ctx _ = pure ctx