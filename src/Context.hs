module Context where
import Syntax

data Context = Context {ids:: [(Name, Type)], defs:: [(Name, Term)]}

index :: Context -> Int -> (Name, Type)
index (Context {ids}) i = ids !! i

lookupId :: Context -> Name -> Maybe Type
lookupId (Context {ids}) name = lookup name ids

addId :: Context -> Name -> Type -> Context
addId ctx n t = ctx {ids= (n,t): ids ctx}

lookupDef :: Context -> Name -> Maybe Term
lookupDef (Context {defs}) name = lookup name defs

addDef :: Context -> Name -> Term -> Context
addDef ctx n t = ctx {defs = (n,t): defs ctx}