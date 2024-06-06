module Main where
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Control.Monad (forM_)
import MlExpr
import Syntax
import Desugar
import Context
import Typecheck (infer)
import Control.Monad.Except (runExceptT)
import Eval (normalize)
import Control.Monad.Reader
import Debug.Trace (traceM)
import Data.Text

runFile name = do
    contents <- T.readFile name
    let ast = parse contents >>= match
    case ast of
        Left err -> putStrLn $ unpack err
        Right e -> do
            result <- runGamma (eval =<< toNameless e)
            case result of
                Left err -> do
                    putStrLn $ unpack err
                Right (t@(Sigma _), v@(Record _)) -> do
                    putStrLn ("signature : " ++ prettySyntax t ++ "\nmodule = " ++ prettySyntax v)
                Right (t, v) -> do
                    putStrLn ("type : " ++ prettySyntax t ++ "\nvalue = " ++ prettySyntax v)
    where
        eval e = do
            (e1, t) <- infer e
            e' <- normalize True e1
            e1' <- toNamed e'
            t' <- toNamed t
            pure (t',e1')
main = do
    args <- getArgs
    forM_ args runFile