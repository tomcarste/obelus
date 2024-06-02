module Context where
import Syntax
import qualified Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Reader
import Control.Monad.Except
data Context 
    = Context {ids:: [(Name, Type)], defs:: [(Name, Term)]}
    deriving Show

lookupId :: Name -> Context -> Maybe Type
lookupId  name (Context {ids}) = lookup name ids

addId :: Name -> Type -> Context -> Context
addId n t ctx = ctx {ids= (n,t): ids ctx}

lookupDef :: Name -> Context -> Maybe Term
lookupDef name (Context {defs}) = lookup name defs

addDef :: Name -> Term -> Context -> Context
addDef n t ctx = ctx {defs = (n,t): defs ctx}

type Gamma = Unbound.FreshMT (ReaderT Context (ExceptT String IO))

runGamma :: Gamma a -> IO (Either String a)
runGamma g = runExceptT $ runReaderT (Unbound.runFreshMT g) (Context {ids=[], defs=[]})
