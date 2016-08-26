module Language.Haskell.Exference.Core.Internal.ExferenceNodeBuilder
  ( SearchNodeBuilder
  , builderSetReason
  , builderAllocVar
  )
where

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.Internal.ExferenceNode
import Language.Haskell.Exference.Core.FunctionBinding
import Control.Monad.State ( State, MonadState, gets )
import Control.Lens

type SearchNodeBuilder a = State SearchNode a

-- sets reason, and, as appropriate, lastNode
builderSetReason :: MonadState SearchNode m => String -> m ()
builderSetReason r = do
  lastStepReason .= r
  previousNode <~ gets Just

builderAllocVar :: MonadState SearchNode m => m TVarId
builderAllocVar = do
  vid <- use nextVarId
  varUses . at vid ?= 0
  nextVarId <<+= 1
