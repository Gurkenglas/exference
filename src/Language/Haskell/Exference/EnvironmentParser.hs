{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.EnvironmentParser
  ( parseModules
  , parseModulesSimple
  , environmentFromModuleAndRatings
  , environmentFromPath
  , haskellSrcExtsParseMode
  , compileWithDict
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.FunctionDecl

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExferenceStats

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>), (<*) )
import Control.Arrow ( (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find, isSuffixOf )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList, catMaybes )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import System.Directory ( getDirectoryContents )
import Control.Exception ( try, SomeException )
import Data.Bifunctor ( first, second )

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    , defaultParseMode )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Data.List.Split ( chunksOf )
import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Language.Haskell.Exference.Core.TypeUtils

import qualified Data.Map as M
import qualified Data.IntMap as IntMap


builtInDeclsM :: (Monad m) => MultiRWST r w s m [HsFunctionDecl]
builtInDeclsM = do
  let consType = (Cons, TypeArrow
            (TypeVar 0)
            (TypeArrow (TypeApp (TypeCons ListCon)
                                (TypeVar 0))
                       (TypeApp (TypeCons ListCon)
                                (TypeVar 0))))
  tupleConss <- mapM (\(a,b) -> [(a,y) | y <- unsafeReadType0 b])
    [ (,) (TupleCon 0) "Unit"
    , (,) (TupleCon 2) "a -> b -> (a, b)"
    , (,) (TupleCon 3) "a -> b -> c -> (a, b, c)"
    , (,) (TupleCon 4) "a -> b -> c -> d -> (a, b, c, d)"
    , (,) (TupleCon 5) "a -> b -> c -> d -> e -> (a, b, c, d, e)"
    , (,) (TupleCon 6) "a -> b -> c -> d -> e -> f -> (a, b, c, d, e, f)"
    , (,) (TupleCon 7) "a -> b -> c -> d -> e -> f -> g -> (a, b, c, d, e, f, g)"
    ]
  return $ consType : tupleConss

builtInDeconstructorsM :: (Monad m) => MultiRWST r w s m [DeconstructorBinding]
builtInDeconstructorsM = mapM helper ds
 where
  helper (t, xs) = [ (x,xs,False)
                   | x <- unsafeReadType0 t
                   ]
  ds = [ (,) "(a, b)" [(TupleCon 2, [TypeVar 0, TypeVar 1])]
       , (,) "(a, b, c)" [(TupleCon 3, [TypeVar 0, TypeVar 1, TypeVar 2])]
       , (,) "(a, b, c, d)" [(TupleCon 4, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3])]
       , (,) "(a, b, c, d, e)" [(TupleCon 5, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3, TypeVar 4])]
       , (,) "(a, b, c, d, e, f)" [(TupleCon 6, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3, TypeVar 4, TypeVar 5])]
       , (,) "(a, b, c, d, e, f, g)" [(TupleCon 7, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3, TypeVar 4, TypeVar 5, TypeVar 6])]
       ]

-- | Takes a list of bindings, and a dictionary of desired
-- functions and their rating, and compiles a list of
-- RatedFunctionBindings.
--
-- If a function in the dictionary is not in the list of bindings,
-- Left is returned with the corresponding name.
--
-- Otherwise, the result is Right.
compileWithDict :: [(QualifiedName, Float)]
                -> [HsFunctionDecl]
                -> Either String [RatedHsFunctionDecl]
                -- function_not_found or all bindings
compileWithDict ratings binds =
  ratings `forM` \(name, rating) ->
    case find ((name==).fst) binds of
      Nothing    -> Left $ show name
      Just (_,t) -> Right (name, rating, t)

-- | input: a list of filenames for haskell modules and the
-- parsemode to use for it.
--
-- output: the environment extracted from these modules, wrapped
-- in a Writer that contains warnings/errors.
parseModules :: forall m r w s
              . ( m ~ MultiRWST r w s IO
                , ContainsType [String] w
                )
             => [(ParseMode, String)]
             -> m
                  ( [HsFunctionDecl]
                  , [DeconstructorBinding]
                  , StaticClassEnv
                  , [QualifiedName]
                  , TypeDeclMap
                  )
parseModules l = do
  rawTuples <- lift $ mapM hRead l
  let eParsed = map hParse rawTuples
  {-
  let h :: Decl -> IO ()
      h i@(InstDecl _ _ _ _ _ _ _) = do
        pprint i >>= print
      h _ = return ()
  forM_ (rights eParsed) $ \(Module _ _ _ _ _ _ ds) ->
    forM_ ds h
  -}
  -- forM_ (rights eParsed) $ \m -> pprintTo 10000 m >>= print
  mapM_ (mTell . (:[])) $ lefts eParsed
  let mods = rights eParsed
  let ds = getDataTypes mods
  typeDeclsE <- getTypeDecls ds mods
  lefts typeDeclsE `forM_` (mTell . (:[]))
  let typeDecls = M.fromList $ (\x -> (tdecl_name x, x)) <$> rights typeDeclsE
  (cntxt@(StaticClassEnv clss insts), n_insts) <- getClassEnv ds typeDecls mods
  -- TODO: try to exfere this stuff
  (decls, deconss) <- do
    stuff <- mapM (hExtractBinds cntxt ds typeDecls) mods
    return $ concat *** concat $ unzip stuff
  let clssNames = fmap tclass_name clss
  let allValidNames = ds ++ clssNames
  let
    dataToBeChecked :: [(String, HsType)]
    dataToBeChecked =
         [ ("the instance data for " ++ show i, t)
         | insts' <- M.elems insts
         , i@(HsInstance _ _ ts) <- insts'
         , t <- ts]
      ++ [ ("the binding " ++ show n, t)
         | (n, t) <- decls]
  let
    check :: String -> HsType -> m ()
    check s t = do
      findInvalidNames allValidNames t `forM_` \n ->
        mTell ["unknown binding '"++show n++"' used in " ++ s]
  dataToBeChecked `forM_` uncurry check
  mTell ["got " ++ show (length clss) ++ " classes"]
  mTell ["and " ++ show (n_insts) ++ " instances"]
  mTell ["(-> " ++ show (length $ concat $ M.elems $ insts) ++ " instances after inflation)"]
  mTell ["and " ++ show (length decls) ++ " function decls"]
  builtInDecls          <- builtInDeclsM
  builtInDeconstructors <- builtInDeconstructorsM
  return $ ( builtInDecls++decls
           , builtInDeconstructors++deconss
           , cntxt
           , allValidNames
           , typeDecls
           )
  where
    hRead :: (ParseMode, String) -> IO (ParseMode, String)
    hRead (mode, s) = (,) mode <$> readFile s
    hParse :: (ParseMode, String) -> Either String Module
    hParse (mode, content) = case parseModuleWithMode mode content of
      f@(ParseFailed _ _) -> Left $ show f
      ParseOk modul       -> Right modul
    hExtractBinds :: StaticClassEnv
                  -> [QualifiedName]
                  -> TypeDeclMap
                  -> Module
                  -> m ([HsFunctionDecl], [DeconstructorBinding])
    hExtractBinds cntxt ds tDeclMap modul@(Module _ (ModuleName _mname) _ _ _ _ _) = do
      -- tell $ return $ mname
      eFromData <- getDataConss (sClassEnv_tclasses cntxt) ds tDeclMap [modul]
      eDecls <- (++)
        <$> getDecls ds (sClassEnv_tclasses cntxt) tDeclMap [modul]
        <*> getClassMethods (sClassEnv_tclasses cntxt) ds tDeclMap [modul]
      mapM_ (mTell . (:[])) $ lefts eFromData ++ lefts eDecls
      -- tell $ map show $ rights ebinds
      let (binds1s, deconss) = unzip $ rights eFromData
          binds2 = rights eDecls
      return $ ( concat binds1s ++ binds2, deconss )

-- | A simplified version of environmentFromModules where the input
-- is just one module, parsed with some default ParseMode;
-- the output is transformed so that all functionsbindings get
-- a rating of 0.0.
parseModulesSimple :: ( ContainsType [String] w
                      )
                   => String
                   -> MultiRWST r w s IO
                        ( [RatedHsFunctionDecl]
                        , [DeconstructorBinding]
                        , StaticClassEnv
                        , [QualifiedName]
                        , TypeDeclMap
                        )
parseModulesSimple s = helper
                   <$> parseModules [(haskellSrcExtsParseMode s, s)]
 where
  addRating (a,b) = (a,0.0,b)
  helper (decls, deconss, cntxt, ds, tdm) = (addRating <$> decls, deconss, cntxt, ds, tdm)

ratingsFromFile :: String -> IO (Either String [(QualifiedName, Float)])
ratingsFromFile = (fmap . first) show . (try :: IO a -> IO (Either SomeException a))
  . fmap (map (\[name, float] -> (parseQualifiedName name, read float)) . chunksOf 2 . words)
  . readFile


-- TODO: add warnings for ratings not applied
environmentFromModuleAndRatings :: ( ContainsType [String] w
                                   )
                                => String
                                -> String
                                -> MultiRWST r w s IO
                                    ( [FunctionBinding]
                                    , [DeconstructorBinding]
                                    , StaticClassEnv
                                    , [QualifiedName]
                                    , TypeDeclMap
                                    )
environmentFromModuleAndRatings s1 s2 = do
  let exts1 = [ TypeOperators
              , ExplicitForAll
              , ExistentialQuantification
              , TypeFamilies
              , FunctionalDependencies
              , FlexibleContexts
              , MultiParamTypeClasses ]
      exts2 = map EnableExtension exts1
      mode = ParseMode (s1++".hs")
                       Haskell2010
                       exts2
                       False
                       False
                       Nothing
                       False
  (decls, deconss, cntxt, ds, tdm) <- parseModules [(mode, s1)]
  r <- lift $ ratingsFromFile s2
  case r of
    Left e -> do
      mTell ["could not parse ratings!",e]
      return ([], [], cntxt, [], tdm)
    Right ratings -> do
      let f (a,b) = declToBinding
                  $ ( a
                    , fromMaybe 0.0 (lookup a ratings)
                    , b
                    )
      return $ (map f decls, deconss, cntxt, ds, tdm)


environmentFromPath :: ( ContainsType [String] w
                       )
                    => FilePath
                    -> MultiRWST r w s IO
                         ( [FunctionBinding]
                         , [DeconstructorBinding]
                         , StaticClassEnv
                         , [QualifiedName]
                         , TypeDeclMap
                         )
environmentFromPath p = do
  files <- lift $ getDirectoryContents p
  let modules = ((p ++ "/")++) <$> filter (".hs" `isSuffixOf`) files
  let ratings = ((p ++ "/")++) <$> filter (".ratings" `isSuffixOf`) files
  (decls, deconss, cntxt, dts, tdm) <- parseModules
    [ (mode, m)
    | m <- modules
    , let mode = haskellSrcExtsParseMode m]
  rResult <- lift $ ratingsFromFile `mapM` ratings
  let rs = [x | Right xs <- rResult, x <- xs]
  sequence_ $ do
    Left err <- rResult
    return $ mTell ["could not parse rating file", err]
  (rs' :: [(QualifiedName, Float)]) <- fmap join $ sequence $ do
    (rName, rVal) <- rs
    return $ do
      dIds <- fmap join $ sequence $ do
        (dName, _) <- decls
        return $ do
          return $ do
            guard (show rName == show dName)
            return (dName, rVal)
      case dIds of
        [] -> do
          mTell ["rating could not be applied: " ++ show rName]
          return []
        [x] ->
          return [x]
        _ -> do
          mTell ["duplicate function: " ++ show rName]
          return []
  let f (a,b) = declToBinding
              $ ( a
                , fromMaybe 0.0 (lookup a rs')
                , b
                )
  return $ (map f decls, deconss, cntxt, dts, tdm)
