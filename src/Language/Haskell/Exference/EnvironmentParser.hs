module Language.Haskell.Exference.EnvironmentParser
  ( environmentFromModules
  , environmentFromModuleSimple
  , environmentFromModuleAndRatings
  , compileWithDict
  , ratingsFromFile
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.FunctionBinding

import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.SimpleDict
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.ExferenceStats

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>), (<*) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    , defaultParseMode )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Control.Arrow ( first )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import qualified Data.Map as M


builtInBindings :: [FunctionBinding]
builtInBindings = map (second $ readConstrainedType emptyClassEnv)
  $ [ (,) "()" "Unit"
    , (,) "(,)" "Tuple2 a b -> INFPATTERN a b"
    , (,) "(,)" "a -> b -> Tuple2 a b"
    , (,) "(,,)" "Tuple3 a b c -> INFPATTERN a b c"
    , (,) "(,,)" "a -> b -> c -> Tuple3 a b c"
    , (,) "(,,,)" "Tuple4 a b c d -> INFPATTERN a b c d"
    , (,) "(,,,)" "a -> b -> c -> d -> Tuple4 a b c d"
    , (,) "(,,,,)" "Tuple5 a b c d e -> INFPATTERN a b c d e"
    , (,) "(,,,,)" "a -> b -> c -> d -> e -> Tuple5 a b c d e"
    , (,) "(,,,,,)" "Tuple6 a b c d e f -> INFPATTERN a b c d e f"
    , (,) "(,,,,,)" "a -> b -> c -> d -> e -> f -> Tuple6 a b c d e f"
    , (,) "(,,,,,,)" "Tuple7 a b c d e f g -> INFPATTERN a b c d e f g"
    , (,) "(,,,,,,)" "a -> b -> c -> d -> e -> f -> g -> Tuple7 a b c d e f g"
    ]

-- | Takes a list of bindings, and a dictionary of desired
-- functions and their rating, and compiles a list of
-- RatedFunctionBindings.
-- 
-- If a function in the dictionary is not in the list of bindings,
-- Left is returned with the corresponding name.
--
-- Otherwise, the result is Right.
compileWithDict :: [(String, Float)]
                -> [FunctionBinding]
                -> Either String [RatedFunctionBinding]
                -- function_not_found or all bindings
compileWithDict ratings binds = forM ratings $ \(name, rating) ->
  case find ((name==).fst) binds of
    Nothing -> Left name
    Just (_,t) -> Right (name, rating, t)

-- | input: a list of filenames for haskell modules and the
-- parsemode to use for it.
--
-- output: the environment extracted from these modules, wrapped
-- in a Writer that contains warnings/errors.
environmentFromModules :: [(ParseMode, String)]
                   -> IO (Writer
                           [String]
                           ([FunctionBinding], StaticClassEnv))
environmentFromModules l = do
  rawTuples <- mapM hRead l
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
  return $ do
    mapM_ (tell.return) $ lefts eParsed
    let mods = rights eParsed
    cntxt@(StaticClassEnv clss insts) <- getClassEnv mods
    tell ["got " ++ show (length clss) ++ " classes"]
    tell ["and " ++ show (length $ concat $ M.elems insts) ++ " instances"]
    binds <- join <$> mapM (hExtractBinds cntxt) mods
    tell ["and " ++ show (length binds) ++ " bindings"]
    return $ (builtInBindings++binds, cntxt)
  where
    hRead :: (ParseMode, String) -> IO (ParseMode, String)
    hRead (mode, s) = (,) mode <$> readFile s
    hParse :: (ParseMode, String) -> Either String Module
    hParse (mode, content) = case parseModuleWithMode mode content of
      f@(ParseFailed _ _) -> Left $ show f
      ParseOk mod -> Right mod
    hExtractBinds :: StaticClassEnv
                  -> Module
                  -> Writer [String] [FunctionBinding]
    hExtractBinds cntxt mod@(Module _ (ModuleName mname) _ _ _ _ _) = do
      -- tell $ return $ mname
      let ebinds = getBindings cntxt mod
                ++ getDataConss mod
                ++ getClassMethods cntxt mod
      mapM_ (tell.return) $ lefts ebinds
      -- tell $ map show $ rights ebinds
      return $ rights ebinds

-- | A simplified version of environmentFromModules where the input
-- is just one module, parsed with some default ParseMode;
-- the output is transformed so that all functionsbindings get
-- a rating of 0.0.
environmentFromModuleSimple :: String
                         -> IO (Writer
                              [String]
                              ( [RatedFunctionBinding]
                              ,  StaticClassEnv) )
environmentFromModuleSimple s = do
  let exts1 = [ TypeOperators
              , ExplicitForAll
              , ExistentialQuantification
              , TypeFamilies
              , FunctionalDependencies
              , FlexibleContexts
              , MultiParamTypeClasses ]
      exts2 = map EnableExtension exts1
      mode = ParseMode (s++".hs")
                       Haskell2010
                       exts2
                       False
                       False
                       Nothing
  r <- environmentFromModules [(mode, s)]
  let f (a,b) = (a,0.0,b)
  return $ first (map f) <$> r


ratingsFromFile :: String -> IO (Either String [(String, Float)])
ratingsFromFile s = do
  content <- readFile s
  let
    parser =
      (many $ try $ do
        spaces
        name <- many1 (noneOf " ")
        space
        spaces
        char '='
        spaces
        minus <- optionMaybe $ char '-'
        a <- many1 digit
        b <- char '.'
        c <- many1 digit
        case minus of
          Nothing -> return (name, read $ a++b:c)
          Just _  -> return (name, read $ '-':a++b:c))
      <* spaces
  return $ case runParser parser () "" content of
    Left e -> Left $ show e
    Right x -> Right x

-- TODO: add warnings for ratings not applied
environmentFromModuleAndRatings :: String
                            -> String
                            -> IO (Writer
                                [String]
                                ( [RatedFunctionBinding]
                                ,  StaticClassEnv) )
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
  w <- environmentFromModules [(mode, s1)]
  r <- ratingsFromFile s2
  return $ do
    (binds, cntxt) <- w
    case r of
      Left e -> do
        tell ["could not parse ratings!",e]
        return ([], cntxt)
      Right x -> do
        let f (a,b) = ( a
                      , fromMaybe 0.0 (lookup a x)
                      , b
                      )
        return $ (map f binds, cntxt)