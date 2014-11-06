{-# LANGUAGE TupleSections #-}

module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getBindings
  , getDataConss
  , getClassMethods
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.Type
import Language.Haskell.Exference.TypeClasses

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad ( join )
import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List ( find )

import Control.Monad.Trans.Maybe



getBindings :: StaticClassEnv -> Module -> [Either String FunctionBinding]
getBindings env (Module _loc _m _pragma _warning _mexp _imp decls)
  = concatMap (either (return.Left) (map Right) . getFromDecls env) decls

getFromDecls :: StaticClassEnv -> Decl -> Either String [FunctionBinding]
getFromDecls env (TypeSig _loc names qtype)
  = insName qtype
  $ ((<$> names) . helper) <$> convertCType env qtype
getFromDecls _ _ = return []

getFromDecls' :: StaticClassEnv
              -> Decl
              -> ConversionMonad [FunctionBinding]
getFromDecls' env (TypeSig _loc names qtype)
  = mapEitherT (fmap $ insName qtype)
  $ (<$> names) . helper <$> convertCTypeInternal env qtype
getFromDecls' _ _ = return []

insName :: Type -> Either String a -> Either String a
insName qtype = either (\x -> Left $ x ++ " in " ++ prettyPrint qtype) Right

helper :: HsConstrainedType -> Name -> FunctionBinding
helper ct x = (hsNameToString x, ct)

-- type ConversionMonad = EitherT String (State (Int, ConvMap))
getDataConss :: Module -> [Either String FunctionBinding]
getDataConss (Module _loc _m _pragma _warning _mexp _imp decls) = do
  DataDecl _loc _newtflag cntxt name params conss _derives <- decls
  let
    rTypeM :: ConversionMonad HsType
    rTypeM = fmap (foldl TypeApp (TypeCons $ hsNameToString name))
           $ mapM pTransform params
    pTransform :: TyVarBind -> ConversionMonad HsType
    pTransform (KindedVar _ _) = left $ "KindedVar"
    pTransform (UnkindedVar n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM :: QualConDecl -> ConversionMonad (FunctionBinding, FunctionBinding)
    typeM (QualConDecl _loc cbindings ccntxt conDecl) =
      case (cntxt, cbindings, ccntxt, conDecl) of
        ([], [], [], ConDecl cname tys) -> do
          convTs <- mapM convertTypeInternal tys
          rtype  <- rTypeM
          let cons   = HsConstrainedType [] (foldr TypeArrow rtype convTs)
          let decons = HsConstrainedType []
                          (TypeArrow
                            rtype
                            (foldl TypeApp (TypeCons "INFPATTERN") convTs))
          return $ (helper cons cname, helper decons cname)
        ([], [], [], x) ->
          left $ "unknown ConDecl: " ++ show x
        ([], _, _, _) ->
          left $ "constraint or existential type for constructor"
        _ ->
          left $ "context in data type"
  let
    addConsMsg = (++) $ hsNameToString name ++ ": "
  let
    consDeconss = mapM (\x -> evalState (runEitherT $ typeM x) (0, M.empty)) conss
  case consDeconss of
    Left  x -> return $ Left $ addConsMsg x
    Right x -> if length conss == 1
      then x >>= \(a,b) -> [Right a, Right b]
      else x >>= \(a,_) -> [Right a]

getClassMethods :: StaticClassEnv -> Module -> [Either String FunctionBinding]
getClassMethods env (Module _loc _m _pragma _warning _mexp _imp decls) =
  do
    ClassDecl _ _ name vars _ cdecls <- decls
    let nameStr = hsNameToString name
    let errorMod = (++) ("class method for "++nameStr++": ")
    let instClass = find ((nameStr==).tclass_name)
                  $ sClassEnv_tclasses env
    case instClass of
      Nothing -> return $ Left $ "unknown type class: "++nameStr
      Just cls -> let
        cnstrA = HsConstraint cls <$> mapM ((TypeVar <$>) . tyVarTransform) vars
        action :: StateT (Int, ConvMap) Identity [Either String [FunctionBinding]]
        action = do
          cnstrE <- runEitherT cnstrA
          case cnstrE of
            Left x -> return [Left x]
            Right cnstr ->
              mapM ( runEitherT
                   . fmap (map (addConstraint cnstr))
                   . getFromDecls' env)
                $ [ d | ClsDecl d <- cdecls ]
        in concatMap (either (return . Left . errorMod)
                             (map Right))
                   $ runIdentity
                   $ evalStateT action (0, M.empty)
  where
    addConstraint :: HsConstraint -> FunctionBinding -> FunctionBinding
    addConstraint c (n, HsConstrainedType constrs t) =
                    (n, HsConstrainedType (c:constrs) t)
