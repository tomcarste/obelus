module Context where
import Syntax
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Text
import Safe
data Context 
    = Context {ids:: [(Atom, Nameless)], defs:: [(Atom, Nameless)]}
    deriving Show

indexId :: Int -> Context -> Maybe (Atom, Nameless)
indexId i (Context {ids}) = atMay ids i

lookupId :: Atom -> Context -> Maybe Nameless
lookupId  atom (Context {ids}) = lookup atom ids

addId :: Atom -> Nameless -> Context -> Context
addId n t ctx = ctx {ids= (n,t): ids ctx}

lookupDef :: Atom -> Context -> Maybe Nameless
lookupDef atom (Context {defs}) = lookup atom defs

addDef :: Atom -> Nameless -> Context -> Context
addDef n t ctx = ctx {defs = (n,t): defs ctx}

type Gamma = StateT Int (ReaderT Context (ExceptT Text IO))

fresh :: MonadState Int m => Text -> m Atom
fresh a = do
    i <- get
    put (i+1)
    pure (a, i)

runGamma :: Gamma a -> IO (Either Text a)
runGamma g = runExceptT $ runReaderT (evalStateT g 0) (Context {ids=[], defs=[]})
