module Mock where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (state), StateT)
import Data.Default (Default (def))
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Test.Hspec

callTypeName :: Name
callTypeName = mkName "Call"

generateMock :: [Name] -> Q [Dec]
generateMock typeClassNames = do
  functions <- join <$> mapM typeclassFunctions typeClassNames
  let deriveClause = DerivClause Nothing []
  let callDataDeclr =
        DataD [] callTypeName [PlainTV (mkName "ret") ()] Nothing (toGadtCtor <$> functions) [deriveClause]
  instances <- join <$> mapM createInstanceOnType typeClassNames
  extras <- staticExtrasForMockCalls
  pure $ [callDataDeclr] ++ extras ++ instances

toMockCtorName :: Name -> Name
toMockCtorName = mkName . ("Mock_" ++) . nameBase

typeclassFunctions :: Name -> Q [(Name, Type)]
typeclassFunctions cls = do
  c <- reify cls
  pure $ case c of
    ClassI (ClassD _ _ _ _ declr) _ -> declr >>= functionName
    _ -> []
  where
    functionName (SigD name typ) = [(name, typ)]
    functionName _ = []

countArgsInType :: Type -> Int
countArgsInType (AppT (AppT ArrowT _) rest) = 1 + countArgsInType rest
countArgsInType _ = 0

toGadtCtor :: (Name, Type) -> Con
toGadtCtor (name, typ) = GadtC [toMockCtorName name] args retType
  where
    (args, retType) = extractArgsAndReturnTypes typ

extractArgsAndReturnTypes :: Type -> ([BangType], Type)
extractArgsAndReturnTypes (AppT (AppT ArrowT typ) rest) = ((noBang, typ) : args, ret)
  where
    (args, ret) = extractArgsAndReturnTypes rest
    noBang = Bang NoSourceUnpackedness NoSourceStrictness
extractArgsAndReturnTypes (AppT _ ret) = ([], AppT (ConT callTypeName) ret)
extractArgsAndReturnTypes ret = ([], AppT (ConT callTypeName) ret)

createInstanceOnType :: Name -> Q [Dec]
createInstanceOnType name =
  typeclassFunctions name >>= createInstance name

createInstance :: Name -> [(Name, Type)] -> Q [Dec]
createInstance name funcs = do
  let ctx = AppT (ConT ''MonadIO) (VarT $ mkName "m")
  let typ = AppT (ConT name) (ConT (mkName "TestM") `AppT` VarT (mkName "a") `AppT` VarT (mkName "m"))
  funcDeclrs <- mapM (\(n, t) -> toInstanceMethodDef n $ countArgsInType t) funcs
  let inst = InstanceD Nothing [ctx] typ funcDeclrs
  pure [inst]

toInstanceMethodDef :: Name -> Int -> Q Dec
toInstanceMethodDef name argCount = do
  let argNames = mkName . ("a" ++) . show <$> [1 .. argCount]
  let callExp = foldl AppE (ConE $ toMockCtorName name) (VarE <$> argNames)
  bodyExp <- instanceMethod callExp
  pure $ FunD name [Clause (VarP <$> argNames) (NormalB bodyExp) []]

instanceMethod :: Exp -> Q Exp
instanceMethod mockExp = do
  [e|
    do
      declarations <- gets (fmap unsafeUnpackDeclr . mockDeclarations)
      let call = $(pure mockExp)
      getMockValue declarations call <$ registerMockCall call
    |]

staticExtrasForMockCalls :: Q [Dec]
staticExtrasForMockCalls =
  [d|
    deriving instance Show ($(conT callTypeName) a)

    deriving instance Eq ($(conT callTypeName) a)

    data CallWrapper where
      CallWrapper :: (Typeable a, Show a, Eq a, Eq ($(conT callTypeName) a)) => $(conT callTypeName) a -> CallWrapper

    data CallMockDeclaration where
      CallMockDeclaration :: (Typeable a, Show a, Eq a, Eq ($(conT callTypeName) a)) => $(conT callTypeName) a -> a -> CallMockDeclaration

    deriving instance Show CallWrapper

    -- deriving instance Eq CallWrapper
    instance Eq CallWrapper where
      (CallWrapper a) == (CallWrapper b) =
        case cast a of
          Just a' -> a' == b
          Nothing -> False

    data MockCalls = MockCalls {calls :: [CallWrapper], mockDeclarations :: [CallMockDeclaration]}

    newtype TestM ret m a = TestM {runTestM :: StateT MockCalls m a}
      deriving (Functor, Applicative, Monad, MonadIO, MonadState MockCalls)

    runTestMWithMocks :: (MonadIO m) => TestM x m a -> m (a, MockCalls)
    runTestMWithMocks action = runStateT (runTestM action) (MockCalls [] [])

    registerMockCall :: (MonadState MockCalls m, Typeable a, Show a, Eq a) => $(conT callTypeName) a -> m ()
    registerMockCall call =
      void $ state (\mock -> ((), mock {calls = calls mock ++ [CallWrapper call]}))

    getMockValue :: (Typeable a, Default a, Show a, Eq a) => [($(conT callTypeName) a, a)] -> $(conT callTypeName) a -> a
    getMockValue [] _ = def
    getMockValue ((fn, ret) : _) call | call == fn = ret
    getMockValue (_ : rest) call = getMockValue rest call

    unsafeUnpackDeclr :: CallMockDeclaration -> ($(conT callTypeName) a, a)
    unsafeUnpackDeclr (CallMockDeclaration f r) = unsafeCoerce (f, r)

    mockReturns :: (MonadState MockCalls m, Typeable a, Show a, Eq a) => $(conT callTypeName) a -> a -> m ()
    mockReturns call ret =
      state (\mock -> ((), mock {mockDeclarations = mockDeclarations mock ++ [CallMockDeclaration call ret]}))

    shouldHaveCalled :: (HasCallStack, Typeable a, Show a, Eq a) => MockCalls -> $(conT callTypeName) a -> Expectation
    shouldHaveCalled mock call = calls mock `shouldContain` [CallWrapper call]

    shouldContainCalls :: (HasCallStack, Typeable a, Show a, Eq a) => MockCalls -> [$(conT callTypeName) a] -> Expectation
    shouldContainCalls mock ls = calls mock `shouldContain` (CallWrapper <$> ls)
    |]
