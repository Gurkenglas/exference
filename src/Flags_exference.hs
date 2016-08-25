module Flags_exference
  ( linkNodes
  )
where

linkNodes :: Bool
#if LINK_NODES
linkNodes = True
#else
linkNodes = False
#endif