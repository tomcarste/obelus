module Repl where

import Syntax
import Context
import Eval
import Typecheck
import MlExpr
import qualified Unbound.Generics.LocallyNameless as Unbound
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
                    case parse (T.pack s) >>= match . Block  of
                        Right es -> do
                            ctx' <- eval es
                            local (const ctx') go
                            
                        Left err -> do
                            liftIO $ print err
                            go
        eval :: Term -> Gamma Context
        eval e = do
            ctx <- ask
            t <- infer e
            e' <- normalize True e
            case (t, e') of
                (Sigma teleTy, v@(Record tele)) -> do
                    (rt, ()) <- Unbound.unbind teleTy
                    (r, ()) <- Unbound.unbind tele
                    liftIO $ putStrLn ("signature : " ++ show t ++ "\nmodule = " ++ show v)
                    foldM addRecord2 ctx (zip (toList rt) (toList r))
                (_, v) -> do
                    liftIO $ putStrLn ("type : " ++ show t ++ "\nvalue = " ++ show v)
                    pure ctx


addRecord2 :: Applicative f => Context -> (Entry, Entry) -> f Context
addRecord2 ctx (Named n (Unbound.Embed e), Named _ (Unbound.Embed t)) = do
    pure $ Context.addId n t (Context.addDef n e ctx)
addRecord2 ctx _ = pure ctx