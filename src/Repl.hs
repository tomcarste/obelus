module Repl where

import Syntax
import Context
import Eval
import Typecheck
import MlExpr
import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Data.Text as T
import Control.Monad.Except (runExceptT)
import Control.Monad (foldM)
import System.Console.Readline
repl :: IO ()
repl = go (Context {ids=[],defs=[]})
    where
        go :: Context -> IO ()
        go ctx =
            do
            l <- readline "%"
            case l of
                Just s -> do
                    let ep = parse (T.pack s)
                    let ee = ep >>= traverse match
                    print (show ep)
                    print (show ee)
                    case ee of
                        Right e -> do
                            ctx' <- foldM eval ctx e 
                            go ctx'
                        Left err -> do
                            print err
                            go ctx
                Nothing -> pure ()
        eval ctx (Seq es) = foldM eval ctx es
        eval ctx (Let n t e) = do
            result <- runExceptT $ Unbound.runFreshMT $ check ctx e t
            case result of
                Right () -> do
                    let v = normalize True ctx e
                    let name = Unbound.name2String n
                    putStrLn (name ++ " : " ++ prettyPrint t ++ "\n" ++ name ++ " = " ++ prettyPrint v)
                    pure $ addDef (addId ctx n t) n v
                Left err -> do
                    print err
                    pure ctx
        eval ctx e = do
            result <- runExceptT $ Unbound.runFreshMT $ infer ctx e
            case result of
                Left err -> print err
                Right t -> do
                    let v = normalize True ctx e
                    putStrLn ("type : " ++ prettyPrint t ++ "\nvalue = " ++ prettyPrint v)
            pure ctx