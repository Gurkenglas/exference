{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}


module Language.Haskell.Exference.Core.Internal.Exference
  ( findExpressions
  , ExferenceHeuristicsConfig (..)
  , ExferenceInput (..)
  , ExferenceOutputElement
  , ExferenceChunkElement
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.Core.SearchTree
import Language.Haskell.Exference.Core.Internal.Unify
import Language.Haskell.Exference.Core.Internal.ConstraintSolver
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder

import Control.Monad.Trans.MultiState

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Sequence as Seq

import Control.DeepSeq.Generics
import System.Mem.StableName ( StableName, makeStableName )
import System.IO.Unsafe ( unsafePerformIO )

import Data.Maybe ( maybeToList, listToMaybe, fromMaybe, catMaybes, mapMaybe, isNothing )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( when, unless, guard, mzero, replicateM
                     , replicateM_, forM, join, forM_ )
import Control.Applicative ( (<$>), (<*>), (*>) )
import Data.List ( partition, sortBy, groupBy, unfoldr )
import Data.Foldable ( traverse_ )
import Data.Ord ( comparing )
import Data.Function ( on )
import Data.Functor ( ($>) )
import Data.Monoid ( mempty, First(First), getFirst, mconcat, Any(..), Endo(..), Sum(..) )
import Data.Foldable ( foldMap, sum, asum )
import Control.Monad.Morph ( lift )
import Data.Typeable ( Typeable )
import Control.Lens

-- import Control.Concurrent.Chan
import Control.Concurrent ( forkIO )
import qualified GHC.Conc.Sync

import qualified ListT

import Data.Data ( Data )

-- import Data.DeriveTH
import Debug.Hood.Observe
import Debug.Trace

import Prelude hiding ( sum )



data ExferenceHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar                :: Float
  , heuristics_goalCons               :: Float
  , heuristics_goalArrow              :: Float
  , heuristics_goalApp                :: Float
  , heuristics_stepProvidedGood       :: Float
  , heuristics_stepProvidedBad        :: Float
  , heuristics_stepEnvGood            :: Float
  , heuristics_stepEnvBad             :: Float
  , heuristics_tempUnusedVarPenalty   :: Float
  , heuristics_tempMultiVarUsePenalty :: Float
  , heuristics_functionGoalTransform  :: Float
  , heuristics_unusedVar              :: Float
  , heuristics_solutionLength         :: Float
  }
  deriving (Show, Data, Typeable)

data ExferenceInput = ExferenceInput
  { input_goalType    :: HsType                 -- ^ try to find a expression
                                                -- of this type
  , input_envFuncs    :: [FunctionBinding]      -- ^ the list of functions
                                                -- that may be used
  , input_envDeconsS  :: [DeconstructorBinding] -- ^ the list of deconstructors
                                                -- that may be used for pattern
                                                -- matching
  , input_envClasses  :: StaticClassEnv
  , input_allowUnused :: Bool                   -- ^ if false, forbid solutions
                                                -- where any bind is unused
  , input_allowConstraints :: Bool              -- ^ if true, allow solutions
                                                -- that have unproven
                                                -- constraints remaining.
  , input_allowConstraintsStopStep :: Int       -- ^ stop ignoring
                                                -- tc-constraints after this
                                                -- step to have some chance to
                                                -- find some solution.
  , input_multiPM     :: Bool                   -- ^ pattern match on
                                                -- multi-constructor data types
                                                -- if true. serverly increases
                                                -- search space (decreases
                                                -- performance).
  , input_qNameIndex  :: QNameIndex             -- ^ the qNameIndex for
                                                -- looking up the ids in
                                                -- expressions.
  , input_maxSteps    :: Int                    -- ^ the maximum number of
                                                -- steps to perform (otherwise
                                                -- would not terminate if
                                                -- there were no (more)
                                                -- solutions).
  , input_memoryLimit :: Maybe Int              -- ^ allows to limit memory
                                                -- usage. no effect if Nothing;
                                                -- for (Just x), memory usage
                                                -- scales with x.
                                                -- Lower memory usage discards
                                                -- states (and, thus, potential
                                                -- solutions).
  , input_heuristicsConfig :: ExferenceHeuristicsConfig
  }
  deriving (Show, Data, Typeable)

type ExferenceOutputElement = (Expression, [HsConstraint], ExferenceStats)
type ExferenceChunkElement = (BindingUsages, SearchTree, [ExferenceOutputElement])

type RatedNodes = Q.MaxPQueue Float SearchNode

type FindExpressionsState = ( Int    -- number of steps already performed
                            , Float  -- worst rating of state in pqueue
                            , BindingUsages
                            , SearchTreeBuilder (StableName SearchNode)
                            , RatedNodes -- pqueue
                            )

-- Entry-point and main function of the algorithm.
-- Takes input, produces list of outputs. Output is basically a
-- [[Solution]], plus some statistics and stuff.
-- Nested list to allow executing n steps even when no solutions are found
-- (e.g. you take 1000, and get only []'s).
--
-- Basic implementation idea: We traverse a search tree. A step (`stateStep`
-- function) evaluates one node, and returns
-- a) new search nodes b) potential solutions.
-- findExpressions does the following stuff:
--   - determine what searchnode to use next (using a priority queue)
--   - call stateStep repeatedly
--   - convert stuff
--   - consider some special abort conditions
findExpressions :: ExferenceInput
                -> [ExferenceChunkElement]
findExpressions (ExferenceInput rawType
                                funcs
                                deconss
                                sClassEnv
                                allowUnused
                                allowConstraints
                                _allowConstraintsStopStep
                                multiPM
                                qNameIndex
                                maxSteps -- since we output a [[x]],
                                         -- this would not really be
                                         -- necessary anymore. but
                                         -- we also use it for calculating
                                         -- memory limit stuff, and it is
                                         -- not worth the refactor atm.
                                memLimit
                                heuristics) =
  take maxSteps $ unfoldr helper $ rootFindExpressionState
 where
  rootFindExpressionState =
    ( 0
    , 0
    , emptyBindingUsages
    , initialSearchTreeBuilder initNodeName (ExpHole 0)
    , Q.singleton 0.0 rootSearchNode
    )
  t = forallify rawType
  rootSearchNode = SearchNode
    (Seq.singleton (VarBinding 0 t, 0))
    []
    initialScopes
    IntMap.empty
    (V.fromList funcs) -- TODO: lift this further up?
    deconss
    (mkQueryClassEnv sClassEnv [])
    (ExpHole 0)
    1 -- TODO: change to 0?
    (largestId t)
    0
    0.0
#if LINK_NODES
    Nothing
#endif
    ""
    Nothing
  initNodeName = unsafePerformIO $ makeStableName $! rootSearchNode
  transformSolutions :: [SearchNode] -> FindExpressionsState -> ExferenceChunkElement
  transformSolutions potentialSolutions (n, _, newBindingUsages, newSearchTreeBuilder, newNodes) = (
      newBindingUsages,
      buildSearchTree newSearchTreeBuilder initNodeName,
      [ (e, remainingConstraints, ExferenceStats n d $ Q.size newNodes)
      | solution <- potentialSolutions
      , let contxt = view node_queryClassEnv solution
      , remainingConstraints <- maybeToList
                              $ filterUnresolved contxt
                              $ view node_constraintGoals solution
        -- if allowConstraints, unresolved constraints are allowed;
        -- otherwise we discard this solution.
      , allowConstraints || null remainingConstraints
      , let unusedVarCount = getUnusedVarCount solution
        -- similarly:
        -- if allowUnused, there may be unused variables in the
        -- output. Otherwise the solution is discarded.
      , allowUnused || unusedVarCount==0
      , let e = -- trace (showNodeDevelopment solution) $
                view node_expression solution
      , let d = view node_depth solution
              + ( heuristics_unusedVar heuristics
                * fromIntegral unusedVarCount
                )
              -- + ( heuristics_solutionLength heuristics
              --   * fromIntegral (length $ show e)<
              --   )
      ]
    )
  helper :: FindExpressionsState -> Maybe (ExferenceChunkElement, FindExpressionsState)
  helper (n, worst, bindingUsages, st, states)
    | Q.null states = Nothing
    | ((_,s), restNodes) <- Q.deleteFindMax states = let
                   -- actual work happens in stateStep
      rNodes = stateStep multiPM
                         allowConstraints
                         qNameIndex
                         heuristics
                         s
      -- distinguish "finished"/"unfinished" sub-SearchNodes
      (potentialSolutions, futures) = partition (Seq.null . view node_goals) rNodes
      newBindingUsages = fromMaybe id (incBindingUsage <$> view node_lastStepBinding s) bindingUsages
      f :: Float -> Float
      f x | x > 900 = 0.0
          | otherwise = let k = 1.111e-3*x
                         in 1 + 2*k**3 - 3*k**2
      ratedNew    = [ ( rateNode heuristics newS + 4.5*f (fromIntegral n)
                      , newS)
                    | newS <- futures ]
      qsize = Q.size states
        -- this cutoff is somewhat arbitrary, and can, theoretically,
        -- distort the order of the results (i.e.: lead to results being
        -- omitted).
      filteredNew = fromMaybe id mFunc ratedNew where
        mFunc = [ filter ((>cutoff) . fst)
                | qsize > maxSteps
                , mmax <- memLimit
                , let cutoff = worst * fromIntegral mmax
                                     / fromIntegral qsize
                ]

      newNodes = foldr (uncurry Q.insert) restNodes filteredNew
      newSearchTreeBuilder =
#if BUILD_SEARCH_TREE
        ((++) [ unsafePerformIO $ do
            n1 <- makeStableName $! ns
            n2 <- makeStableName $! s
            return (n1,n2,view node_expression ns)
          | ns<-rNodes] &&&
        (:) (unsafePerformIO (makeStableName $! s)))
#endif
        st
      newState =
        ( n+1
        , minimum $ worst:map fst filteredNew
        , newBindingUsages
        , newSearchTreeBuilder
        , newNodes )
      in Just (transformSolutions potentialSolutions newState, newState)

rateNode :: ExferenceHeuristicsConfig -> SearchNode -> Float
rateNode h s = 0.0 - rateGoals h (view node_goals s) - view node_depth s + rateUsage h s
 -- + 0.6 * rateScopes (view node_providedScopes s)

rateGoals :: ExferenceHeuristicsConfig -> Seq.Seq TGoal -> Float
rateGoals h = sum . fmap rateGoal
  where
    rateGoal (VarBinding _ t,_) = tComplexity t
    -- TODO: actually measure performance with different values,
    --       use derived values instead of (arbitrarily) chosen ones.
    tComplexity (TypeVar _)         = heuristics_goalVar h
    tComplexity (TypeConstant _)    = heuristics_goalCons h -- TODO different heuristic?
    tComplexity (TypeCons _)        = heuristics_goalCons h
    tComplexity (TypeArrow t1 t2)   = heuristics_goalArrow h + tComplexity t1 + tComplexity t2
    tComplexity (TypeApp   t1 t2)   = heuristics_goalApp h   + tComplexity t1 + tComplexity t2
    tComplexity (TypeForall _ _ t1) = tComplexity t1

-- using this rating had bad effect on ordering; not used anymore
{-
rateScopes :: Scopes -> Float
rateScopes (Scopes _ sMap) = alaf Sum foldMap f sMap where
    f (Scope binds _) = fromIntegral (length binds)
-}

rateUsage :: ExferenceHeuristicsConfig -> SearchNode -> Float
rateUsage h s = alaf Sum foldMap f $ s ^.. node_varUses . folded where
  f :: Int -> Float
  f 0 = - heuristics_tempUnusedVarPenalty h
  f 1 = 0
  f n = - fromIntegral (n-1) * heuristics_tempMultiVarUsePenalty h

getUnusedVarCount :: SearchNode -> Int
getUnusedVarCount s = length $ filter (==0) $ s ^.. node_varUses . folded

-- just redirects to stateStep2; purpose is debugging. i should use the
-- `| False -> debugging stuff` trick instead maybe.
stateStep :: Bool
          -> Bool
          -> QNameIndex
          -> ExferenceHeuristicsConfig
          -> SearchNode
          -> [SearchNode]
stateStep multiPM allowConstrs qNameIndex h s
  = stateStep2 multiPM allowConstrs qNameIndex h
    -- $ trace (showSearchNode' qNameIndex s ++ " " ++ show (rateNode h s))
    $ s
    -- trace (unwords [ show (view  node_depth                     s)
    --                , show (views node_goals          rateGoals  s)
    --                , show (views node_providedScopes rateScopes s)
    --                , show (view  node_expression                s))]) $

-- Take one SearchNode, return some amount of sub-SearchNodes. Some of the
-- returned SearchNodes may in fact be (potential) solutions that do not
-- require further evaluation.
--
-- Basic implementation idea:
-- Take the first goal for this SearchNode. Its type determines what the next
-- step is (and which sub-function to use).
stateStep2 :: Bool
           -> Bool
           -> QNameIndex
           -> ExferenceHeuristicsConfig
           -> SearchNode
           -> [SearchNode]
stateStep2 multiPM allowConstrs qNameIndex h s
  | view node_depth s > 200.0 = []
  | TypeArrow _ _      <- goalType = [ modifyNodeBy s' $ arrowStep goalType [] ]
  | TypeForall is cs t <- goalType = [ modifyNodeBy s' $ forallStep is cs t ]
  | otherwise = byProvided ++ byFunctionSimple
  where

    ((VarBinding var goalType, scopeId) Seq.:< gr) = Seq.viewl $ view node_goals s
    s' = node_goals .~ gr $ s

    -- if type is TypeArrow, transform to lambda expression.
    arrowStep :: HsType -> [VarBinding] -> SearchNodeBuilder ()
    arrowStep g ts
      -- descend until no more TypeArrows, accumulating what is seen.
      | TypeArrow t1 t2 <- g = do
          nextId <- builderAllocVar
          arrowStep t2 (VarBinding nextId t1 : ts)
      -- finally, do the goal/expression transformation.
      | otherwise = do
          nextId <- node_nextVarId <<+= 1
          newScopeId <- builderAddScope scopeId
          node_expression %= fillExprHole var
            (foldl (\e (VarBinding v ty) -> ExpLambda v ty e) (ExpHole nextId) ts)
          node_depth += heuristics_functionGoalTransform h
          builderSetReason "function goal transform"
          node_lastStepBinding .= Nothing
          -- for each parameter introduced in the lambda-expression above,
          -- it may be possible to pattern-match. and pattern-matching
          -- may cause duplication of the goals (e.g. for the different cases
          -- in the pattern match).
          additionalGoals <- (addScopePatternMatch multiPM g nextId newScopeId
            $ map splitBinding
            $ reverse
            $ ts
            )
          node_goals <>= Seq.fromList additionalGoals

    -- if type is TypeForall, fix the forall-variables, i.e. invent a fresh
    -- set of constants that replace the relevant forall-variables.
    forallStep :: [TVarId] -> [HsConstraint] -> HsType -> SearchNodeBuilder ()
    forallStep vs cs t = do
      dataIds <- mapM (const $ node_nextNVarId <<+= 1) vs
      node_depth += heuristics_functionGoalTransform h -- TODO: different heuristic?
      builderSetReason "forall-type goal transformation"
      node_lastStepBinding .= Nothing
      let substs = IntMap.fromList $ zip vs $ TypeConstant <$> dataIds
      node_goals %= ((VarBinding var $ snd $ applySubsts substs t, scopeId) <|)
      node_queryClassEnv %= addQueryClassEnv (snd . constraintApplySubsts substs <$> cs)
    -- try to resolve the goal by looking at the parameters in scope, i.e.
    -- the parameters accumulated by building the expression so far.
    -- e.g. for (\x -> (_ :: Int)), the goal can be filled by `x` if
    -- `x :: Int`.

    byProvided = flip mapMaybe
      (scopeGetAllBindings (view node_providedScopes s) scopeId)
      $ \(provId, provT, provPs, forallTypes, constraints) -> let
          incF        = incVarIds (+(1 + view node_maxTVarId s))
          ss          = IntMap.fromList $ zip forallTypes (incF . TypeVar <$> forallTypes)
          provType    = snd $ applySubsts ss provT
          provConstrs = S.toList $ S.union
            (qClassEnv_constraints $ view node_queryClassEnv s)
            (S.fromList (snd . constraintApplySubsts ss <$> constraints))
        in byGenericUnify
          (Right (provId, foldr TypeArrow provT provPs))
          (provType)
          (provConstrs)
          (snd . applySubsts ss <$> provPs)
          (heuristics_stepProvidedGood h)
          (heuristics_stepProvidedBad h)
          ("inserting given value " ++ show provId ++ "::" ++ show provT)
          (unify goalType provType)

    -- try to resolve the goal by looking at functions from the environment.
    byFunctionSimple = flip mapMaybe
      (V.toList $ view node_functions s)
      $ \(funcR, funcId, funcRating, funcConstrs, funcParams) -> let
          offset   = 1 + view node_maxTVarId s
          incF     = incVarIds (+offset)
          provType = incF funcR
        in byGenericUnify
          (Left funcId)
          (provType)
          (map (constraintMapTypes incF) funcConstrs)
          (map incF funcParams)
          (heuristics_stepEnvGood h + funcRating)
          (heuristics_stepEnvBad h + funcRating)
          ("applying function " ++ show funcId)
          (unifyOffset goalType (HsTypeOffset funcR offset))

    -- common code for byProvided and byFunctionSimple
    byGenericUnify :: Either QNameId (TVarId, HsType)
                   -> HsType
                   -> [HsConstraint]
                   -> [HsType]
                   -> Float
                   -> Float
                   -> String
                   -> Maybe (Substs, Substs)
                   -> Maybe SearchNode
    byGenericUnify applier
                   provided
                   provConstrs
                   dependencies
                   depthModMatch
                   depthModNoMatch
                   reasonPart
                   unifyResult
      = case unifyResult of
          Nothing               -> noUnify
          Just (goalSS, provSS) -> byUnified goalSS provSS
     where
      applierl = applier ^? _Left
      applierr = applier ^? _Right
      coreExp = either ExpName (uncurry ExpVar) applier
      noUnify = case dependencies of
        [] -> Nothing -- we can't (randomly) partially apply a non-function
        (d:ds) -> Just $ modifyNodeBy s' $ do
          vResult <- builderAllocVar
          vParam  <- node_nextVarId <<+= 1
          node_expression %= fillExprHole var (ExpLet
            vResult
            provided
            (ExpApply coreExp $ ExpHole vParam)
            (ExpHole var))
          node_goals %= ((VarBinding vParam d, scopeId) <|)
          newScopeId <- builderAddScope scopeId
          node_constraintGoals <>= provConstrs
          traverse_ (\r -> node_varUses . singular (ix $ fst r) += 1) applierr
          node_maxTVarId %= max (maximum $ map largestId dependencies)
          node_depth += depthModNoMatch
          builderSetReason $ "randomly trying to apply function "
                            ++ showExpressionPure qNameIndex coreExp
          additionalGoals <- addScopePatternMatch
            multiPM
            goalType
            var
            newScopeId
            (let (r,ps,fs,cs) = splitArrowResultParams provided
              in [(vResult, r, ds++ps, fs, cs)])
          node_goals <>= Seq.fromList additionalGoals
      byUnified goalSS provSS = do
        let allSS = IntMap.union goalSS provSS
            substs = case applier of
              Left _  -> goalSS
              Right _ -> allSS
            contxt = view node_queryClassEnv s
            (applied1, constrs1) = mapM (constraintApplySubsts substs)
                                        (view node_constraintGoals s)
            constrs2 = map (snd . constraintApplySubsts provSS)
                     $ provConstrs
        newConstraints <- if allowConstrs
          then Just $ constrs1 ++ constrs2
          else if getAny applied1
            then                   isPossible contxt (constrs1 ++ constrs2)
            else (constrs1 ++) <$> isPossible contxt constrs2
        Just $ modifyNodeBy s' $ do
          let paramN = length dependencies
          vars <- replicateM paramN $ node_nextVarId <<+= 1
          let newGoals = mkGoals scopeId $ zipWith VarBinding vars dependencies
          node_goals <>= Seq.fromList
            (ala Endo foldMap (applierl $> (goalApplySubst provSS))
            <$> newGoals)
          builderApplySubst substs
          node_expression %= fillExprHole var
            (foldl ExpApply coreExp (map ExpHole vars))
          traverse_ (\r -> node_varUses . singular (ix $ fst r) += 1) applierr
          node_constraintGoals .= newConstraints
          node_maxTVarId %= max (maximum
            $ largestSubstsId goalSS : map largestId dependencies)
          node_depth += depthModMatch
          let substsTxt   = show (IntMap.union goalSS provSS)
                            ++ " unifies "
                            ++ show goalType
                            ++ " and "
                            ++ show provided
          let provableTxt = "constraints (" ++ show (constrs1++constrs2)
                                            ++ ") are provable"
          builderSetReason $ reasonPart ++ ", because " ++ substsTxt
                            ++ " and because " ++ provableTxt
          node_lastStepBinding .= fmap show applierl


{-# INLINE addScopePatternMatch #-}
-- Insert pattern-matching on newly introduced VarPBindings where
-- possible/necessary. Note that this also effectively transforms a goal
-- (into potentially multiple goals), as goal id + HsType + ScopeId = TGoal.
-- So the input describes one single TGoal.
-- TGoals are duplicated when the pattern-matching involves more than one case,
-- as the goals for different cases are distinct because their scopes are
-- modified when new bindings are added by the pattern-matching.
addScopePatternMatch :: Bool -- should p-m on anything but newtypes?
                     -> HsType -- the current goal (should be returned in one
                               --  form or another)
                     -> Int    -- goal id (hole id)
                     -> ScopeId -- scope for this goal
                     -> [VarPBinding]
                     -> SearchNodeBuilder [TGoal]
addScopePatternMatch multiPM goalType vid sid bindings = case bindings of
  []                                    -> return [(VarBinding vid goalType, sid)]
  (b@(v,vtResult,vtParams,_,_):bindingRest) -> do
    offset <- builderGetTVarOffset
    let incF = incVarIds (+offset)
    let expVar = ExpVar v (foldr TypeArrow vtResult vtParams)
    node_providedScopes %= scopesAddPBinding sid b
    let defaultHandleRest = addScopePatternMatch multiPM goalType vid sid bindingRest
    case vtResult of
      TypeVar {}    -> defaultHandleRest -- dont pattern-match on variables, even if it unifies
      TypeArrow {}  ->
        error $ "addScopePatternMatch: TypeArrow: " ++ show vtResult  -- should never happen, given a pbinding..
      TypeForall {} ->
        error $ "addScopePatternMatch: TypeForall (RankNTypes not yet implemented)" -- todo when we do RankNTypes
                ++ show vtResult
      _ | not $ null vtParams -> defaultHandleRest
        | otherwise -> do
          deconss <- use node_deconss
          fromMaybe defaultHandleRest . asum . map mapFunc $ deconss
         where
          mapFunc :: DeconstructorBinding -> Maybe (SearchNodeBuilder [TGoal])
          mapFunc (matchParam, [(matchId, matchRs)], False) = let
            resultTypes = map incF matchRs
            unifyResult = unifyRightOffset vtResult
                                           (HsTypeOffset matchParam offset)
            -- inputType = incF matchParam
            mapFunc1 substs = do -- SearchNodeBuilder
              vars <- replicateM (length matchRs) builderAllocVar
              node_varUses . singular (ix v) += 1
              builderSetReason $ "pattern matching on " ++ showVar v
              let newProvTypes = map (snd . applySubsts substs) resultTypes
                  newBinds = zipWith (\x y -> splitBinding (VarBinding x y))
                                     vars
                                     newProvTypes
                  expr = ExpLetMatch matchId
                                     (zip vars matchRs)
                                     expVar
                                     (ExpHole vid)
              node_expression %= fillExprHole vid expr
              when (not $ null matchRs) $
                node_maxTVarId %= max (maximum $ map largestId newProvTypes)
              addScopePatternMatch multiPM
                                   goalType
                                   vid
                                   sid
                                   (reverse newBinds ++ bindingRest)
            in mapFunc1 <$> unifyResult
          mapFunc (matchParam, matchers@(_:_), False) | multiPM = let
            unifyResult = unifyRightOffset vtResult
                                           (HsTypeOffset matchParam offset)
            -- inputType = incF matchParam
            mapFunc2 substs = do -- SearchNodeBuilder
              mData <- matchers `forM` \(matchId, matchRs) -> do -- SearchNodeBuilder
                newSid <- builderAddScope sid
                let resultTypes = map incF matchRs
                vars <- replicateM (length matchRs) builderAllocVar
                node_varUses . singular (ix v) += 1
                newVid <- node_nextVarId <<+= 1
                let newProvTypes = map (snd . applySubsts substs) resultTypes
                    newBinds = zipWith (\x y -> splitBinding (VarBinding x y)) vars newProvTypes
                when (not $ null matchRs) $
                  node_maxTVarId %= max (maximum $ map largestId newProvTypes)
                return ( (matchId, zip vars newProvTypes, ExpHole newVid)
                       , (newVid, reverse newBinds, newSid) )
              builderSetReason $ "pattern matching on " ++ showVar v
              node_expression %= fillExprHole vid (ExpCaseMatch expVar $ map fst mData)
              concat <$> map snd mData `forM` \(newVid, newBinds, newSid) ->
                addScopePatternMatch multiPM goalType newVid newSid (newBinds++bindingRest)
            in mapFunc2 <$> unifyResult
          mapFunc _ = Nothing -- TODO: decons for recursive data types
  -- where
  --  (<&>) = flip (<$>)
