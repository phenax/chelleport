module Mock where

import Control.Monad (join)
import Language.Haskell.TH

typeclassFunctions :: Name -> Q [(Name, Type)]
typeclassFunctions cls = do
  c <- reify cls
  pure $ case c of
    ClassI (ClassD _ _ _ _ declr) _ -> declr >>= functionName
    _ -> []
  where
    functionName (SigD name typ) = [(name, typ)]
    functionName _ = []

generateMock :: [Name] -> Q [Dec]
generateMock clss = do
  functions <- join <$> mapM typeclassFunctions clss
  pure [DataD [] (mkName "Call") [] Nothing (toCtor <$> functions) [deriveClause]]
  where
    deriveClause = DerivClause Nothing (ConT <$> [''Show, ''Eq])

    toCtor :: (Name, Type) -> Con
    toCtor (name, typ) = NormalC (toCtorName name) ((noBang,) <$> getArgs typ)

    getArgs :: Type -> [Type]
    getArgs (AppT (AppT ArrowT typ) rest) = typ : getArgs rest
    getArgs _ = []

    noBang = Bang NoSourceUnpackedness NoSourceStrictness

    toCtorName :: Name -> Name
    toCtorName = mkName . ("Mock_" ++) . nameBase
