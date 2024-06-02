module Main where
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Control.Monad (forM_)
import MlExpr
import Syntax
import Context
import Typecheck (infer)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Except (runExceptT)
import Eval (normalize)
import Control.Monad.Reader

runFile name = do
    contents <- T.readFile name
    let ast = parse contents >>= match . Block
    print $ show ast
    case ast of
        Left err -> print err
        Right e -> do
            result <- runGamma (eval e)
            case result of
                Left err -> do
                    print err
                Right (t@(Sigma _), v@(Record _)) -> do
                    putStrLn ("signature : " ++ prettyPrint t ++ "\nmodule = " ++ prettyPrint v)
                Right (t, v) -> do
                    putStrLn ("type : " ++ prettyPrint t ++ "\nvalue = " ++ prettyPrint v)
    where
        eval e = do
            t <- infer e
            e' <- normalize True e
            pure (t,e)
main = do
    args <- getArgs
    forM_ args runFile