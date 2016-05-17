{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Exference.Core.Internal.ExferenceNode
  ( SearchNode (..)
  , HasSearchNode (..) -- SearchNode lenses
  , TGoal
  , Scopes (..)
  , Scope (..)
  , ScopeId
  , VarPBinding
  , VarBinding (..)
  , VarUsageMap
  , varBindingApplySubsts
  , varPBindingApplySubsts
  , goalApplySubst
  , showNodeDevelopment
  , scopesApplySubsts
  , mkGoals
  , addScope
  , scopeGetAllBindings
  , scopesAddPBinding
  , splitBinding
  , addNewScopeGoal
  , initialScopes
  , addGoalProvided -- unused atm
  , showSearchNode
  , showSearchNode'
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.FunctionBinding

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import Data.Sequence
import Data.Foldable ( toList )
import Data.Functor.Identity ( runIdentity )

import Text.PrettyPrint

import Control.Arrow ( first, second, (***) )

import Control.DeepSeq.Generics
import Control.DeepSeq
import GHC.Generics
import Control.Lens.TH ( makeClassy )

import Control.Monad.Trans.MultiRWS
import Control.Monad.Trans.MultiState ( runMultiStateTNil )
import Data.HList.ContainsType

import Debug.Hood.Observe



data VarBinding = VarBinding {-# UNPACK #-} !TVarId HsType
 deriving Generic
type VarPBinding = (TVarId, HsType, [HsType], [TVarId], [HsConstraint])
                -- var, result, params, forallTypes, constraints


varBindingApplySubsts :: Substs -> VarBinding -> VarBinding
varBindingApplySubsts substs (VarBinding v t) =
  VarBinding v (snd $ applySubsts substs t)

varPBindingApplySubsts :: Substs -> VarPBinding -> VarPBinding
varPBindingApplySubsts ss (v,rt,pt,fvs,cs) =
  let
    relevantSS = foldr IntMap.delete ss fvs
    (newResult, params, newForalls, newCs) = splitArrowResultParams
                                           $ snd
                                           $ applySubsts relevantSS rt
  in
  ( v
  , newResult
  , map (snd . applySubsts relevantSS) pt ++ params
  , newForalls ++ fvs
  , cs ++ newCs
  )

type ScopeId = Int

data Scope = Scope [VarPBinding] [ScopeId]
            -- scope bindings,   superscopes
  deriving Generic
data Scopes = Scopes ScopeId (IntMap.IntMap Scope)
  deriving Generic
              -- next id     scopes

initialScopes :: Scopes
initialScopes = Scopes 1 (IntMap.singleton 0 $ Scope [] [])

scopeGetAllBindings :: Scopes -> Int -> [VarPBinding]
scopeGetAllBindings ss@(Scopes _ scopeMap) sid =
  case IntMap.lookup sid scopeMap of
    Nothing -> []
    Just (Scope binds ids) -> binds ++ concatMap (scopeGetAllBindings ss) ids

scopesApplySubsts :: Substs -> Scopes -> Scopes
scopesApplySubsts substs (Scopes i scopeMap) = Scopes i $ IntMap.map scopeF scopeMap
  where
    scopeF (Scope binds ids) = Scope (map bindF binds) ids
    bindF = varPBindingApplySubsts substs

{-
scopesAddBinding :: ScopeId -> VarBinding -> Scopes -> Scopes
scopesAddBinding sid binding scopes =
  scopesAddPBinding sid (splitBinding binding) scopes
-}

scopesAddPBinding :: ScopeId -> VarPBinding -> Scopes -> Scopes
scopesAddPBinding sid binding (Scopes nid sMap) = Scopes nid newMap
  where
    newMap = IntMap.adjust addBinding sid sMap
    addBinding :: Scope -> Scope
    addBinding (Scope vbinds ids) = Scope (binding:vbinds) ids

addScope :: ScopeId -> Scopes -> (ScopeId, Scopes)
addScope parent (Scopes nid sMap) = (nid, Scopes (nid+1) newMap)
  where
    newMap   = IntMap.insert nid newScope sMap
    newScope = Scope [] [parent]

type VarUsageMap = IntMap.IntMap Int

type TGoal = (VarBinding, ScopeId)
           -- goal,    id of innermost scope available

goalApplySubst :: Substs -> TGoal -> TGoal
goalApplySubst ss | IntMap.null ss = id
                  | otherwise      = first (varBindingApplySubsts ss)

-- takes a new goal data, and a new set of provided stuff, and
-- returns some actual goal/newScope pair
addGoalProvided :: ScopeId
                -> VarBinding   -- goal binding
                -> [VarBinding] -- new-given-bindings
                -> Scopes
                -> (TGoal, Scopes)
addGoalProvided sid goalBind givenBinds (Scopes nid sMap) =
    ((goalBind, nid),Scopes (nid+1) newMap)
  where
    newMap = IntMap.insert nid (Scope transformedBinds [sid]) sMap
    transformedBinds = map splitBinding givenBinds

addNewScopeGoal :: ScopeId -> VarBinding -> Scopes -> (TGoal, ScopeId, Scopes)
addNewScopeGoal sid goalBind (Scopes nid sMap) =
    ((goalBind, nid), nid, Scopes (nid+1) newMap)
  where
    newMap = IntMap.insert nid (Scope [] [sid]) sMap

mkGoals :: ScopeId
        -> [VarBinding]
        -> [TGoal]
mkGoals sid vbinds = [(b,sid)|b<-vbinds]

data SearchNode = SearchNode
  { _node_goals           :: Seq TGoal
  , _node_constraintGoals :: [HsConstraint]
  , _node_providedScopes  :: Scopes
  , _node_varUses         :: VarUsageMap
  , _node_functions       :: (V.Vector FunctionBinding)
  , _node_deconss         :: [DeconstructorBinding]
  , _node_queryClassEnv   :: QueryClassEnv
  , _node_expression      :: Expression
  , _node_nextVarId       :: {-# UNPACK #-} !TVarId
  , _node_maxTVarId       :: {-# UNPACK #-} !TVarId
  , _node_nextNVarId      :: {-# UNPACK #-} !TVarId -- id used when resolving rankN-types
  , _node_depth           :: {-# UNPACK #-} !Float
#if LINK_NODES
  , _node_previousNode    :: Maybe SearchNode
#endif
  , _node_lastStepReason  :: String
  , _node_lastStepBinding :: (Maybe String)
  }
  deriving Generic

instance NFData VarBinding   where rnf = genericRnf
instance NFData Scope        where rnf = genericRnf
instance NFData Scopes       where rnf = genericRnf
instance NFData SearchNode   where rnf = genericRnf

-- instance Show SearchNode where
--   show (SearchNode sgoals
--               scgoals
--               (Scopes _ scopeMap)
--               _svarUses
--               _sfuncs
--               _sdeconss
--               qClassEnv
--               sexpression
--               snextVarId
--               smaxTVarId
--               snextNVarId
--               sdepth
-- #if LINK_NODES
--               _prev
-- #endif
--               reason
--               _lastStepBinding
--               )
--     = show
--     $ text "SearchNode" <+> (
--           (text   "goals      ="
--            <+> brackets (vcat $ punctuate (text ", ") $ map tgoal $ toList sgoals)
--           )
--       $$  (text $ "constrGoals= " ++ show scgoals)
--       $$  (text   "scopes     = "
--            <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ IntMap.toList scopeMap)
--           )
--       $$  (text $ "classEnv   = " ++ show qClassEnv)
--       $$  (text $ "expression = " ++ showExpression sexpression)
--       $$  (text $ "reason     = " ++ reason)
--       $$  (parens $    (text $ "nextVarId="++show snextVarId)
--                    <+> (text $ "maxTVarId="++show smaxTVarId)
--                    <+> (text $ "nextNVarId="++show snextNVarId)
--                    <+> (text $ "depth="++show sdepth))
--     )
--     where
--       tgoal :: TGoal -> Doc
--       tgoal (vt,scopeId) =  tVarType vt
--                          <> text (" in " ++ show scopeId)
--       tScope :: (ScopeId, Scope) -> Doc
--       tScope (sid, Scope binds supers) =
--             text (show sid ++ " ")
--         <+> parens (text $ show $ supers)
--         <+> text " " <+> brackets (
--                               hcat $ punctuate (text ", ")
--                                                (map tVarPType binds)
--                             )
--       tVarType :: (TVarId, HsType) -> Doc
--       tVarType (i, t) = text $ showVar i ++ " :: " ++ show t
--       tVarPType :: (TVarId, HsType, [HsType], [TVarId], [HsConstraint]) -> Doc
--       tVarPType (i, t, ps, [], []) = tVarType (i, foldr TypeArrow t ps)
--       tVarPType (i, t, ps, fs, cs) = tVarType (i, TypeForall fs cs (foldr TypeArrow t ps))

showSearchNode' :: QNameIndex -> SearchNode -> String
showSearchNode' ind sn = runIdentity
                       $ runMultiRWSTNil
                       $ withMultiStateA ind
                       $ showSearchNode sn

showSearchNode :: ( ContainsType QNameIndex s, Monad m, Functor m )
               => SearchNode
               -> MultiRWST r w s m String
showSearchNode
  (SearchNode sgoals
              scgoals
              (Scopes _ scopeMap)
              _svarUses
              _sfuncs
              _sdeconss
              qClassEnv
              sexpression
              snextVarId
              smaxTVarId
              snextNVarId
              sdepth
#if LINK_NODES
              _prev
#endif
              reason
              _lastStepBinding
              )
    = do
  exprStr <- showExpression sexpression
  return
    $ show
    $ text "SearchNode" <+> (
          (text   "goals      ="
           <+> brackets (vcat $ punctuate (text ", ") $ map tgoal $ toList sgoals)
          )
      $$  (text $ "constrGoals= " ++ show scgoals)
      $$  (text   "scopes     = "
           <+> brackets (vcat $ punctuate (text ", ") $ map tScope $ IntMap.toList scopeMap)
          )
      $$  (text $ "classEnv   = " ++ show qClassEnv)
      $$  (text $ "expression = " ++ exprStr)
      $$  (text $ "reason     = " ++ reason)
      $$  (parens $    (text $ "nextVarId="++show snextVarId)
                   <+> (text $ "maxTVarId="++show smaxTVarId)
                   <+> (text $ "nextNVarId="++show snextNVarId)
                   <+> (text $ "depth="++show sdepth))
    )
 where
  tgoal :: TGoal -> Doc
  tgoal (vt,scopeId) =  tVarType vt
                     <> text (" in " ++ show scopeId)
  tScope :: (ScopeId, Scope) -> Doc
  tScope (sid, Scope binds supers) =
        text (show sid ++ " ")
    <+> parens (text $ show $ supers)
    <+> text " " <+> brackets (
                          hcat $ punctuate (text ", ")
                                           (map tVarPType binds)
                        )
  tVarType :: VarBinding -> Doc
  tVarType (VarBinding i t) = text $ showVar i ++ " :: " ++ show t
  tVarPType :: (TVarId, HsType, [HsType], [TVarId], [HsConstraint]) -> Doc
  tVarPType (i, t, ps, [], []) = tVarType $ VarBinding i (foldr TypeArrow t ps)
  tVarPType (i, t, ps, fs, cs) = tVarType $ VarBinding i (TypeForall fs cs (foldr TypeArrow t ps))

showNodeDevelopment :: forall m
                    . MonadMultiState QNameIndex m
                    => SearchNode
                    -> m String
#if LINK_NODES
showNodeDevelopment s = case node_previousNode s of
  Nothing -> showSearchNode s
  Just p  -> do
    pStr <- showNodeDevelopment p
    cStr <- showSearchNode s
    return $ pStr ++ "\n" ++ cStr
#else
showNodeDevelopment _ = return "[showNodeDevelopment: exference-core was not compiled with -fLinkNodes]"
#endif

-- instance Observable SearchNode where
--   observer state = observeOpaque (show state) state

splitBinding :: VarBinding -> VarPBinding
splitBinding (VarBinding v t) = let (rt,pts,fvs,cs) = splitArrowResultParams t in (v,rt,pts,fvs,cs)

makeClassy ''SearchNode
