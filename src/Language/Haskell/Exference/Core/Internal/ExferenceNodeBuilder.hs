{-# LANGUAGE PartialTypeSignatures #-}

module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( SearchNodeBuilder
  , builderSetReason
  , builderGetTVarOffset
  , builderApplySubst
  , builderAllocVar
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.FunctionBinding

import Control.Monad.State ( State
                           , StateT( StateT )
                           , execState
                           , modify
                           , get
                           , put
                           , gets
                           , state
                           )
import Control.Monad.State.Lazy ( MonadState )
import Control.Applicative
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import Control.Monad ( liftM )

import Control.Lens



type SearchNodeBuilder a = State SearchNode a

{-
builderAddVars :: [TVarId] -> SearchNodeBuilder ()
builderAddVars = (varUses <>=) . M.fromList . map (,0)
-}

-- sets reason, and, as appropriate, lastNode
builderSetReason :: MonadState SearchNode m => String -> m ()
builderSetReason r = do
  lastStepReason .= r
  previousNode <~ gets Just

builderGetTVarOffset :: MonadState SearchNode m => m TVarId
builderGetTVarOffset = uses maxTVarId (+1)
 -- TODO: is (+1) really necessary? it was in pre-transformation code,
 --       but i cannot find good reason now.. test?

builderAllocVar :: MonadState SearchNode m => m TVarId
builderAllocVar = do
  vid <- use nextVarId
  varUses . at vid ?= 0
  nextVarId <<+= 1

-- apply substs in goals and scopes
-- not contraintGoals, because that's handled by caller
builderApplySubst :: MonadState SearchNode m => Substs -> m ()
builderApplySubst substs = do
  goals . mapped %= goalApplySubst substs
  providedScopes %= scopesApplySubsts substs
