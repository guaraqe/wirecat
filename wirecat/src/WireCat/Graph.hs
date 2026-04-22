{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.Graph
  ( NodeId,
    AttrName,
    TypeName,
    Node (..),
    Plug (..),
    Edge (..),
    Graph (..),
    toGraph,
  )
where

import WireCat.Free
import WireCat.Label (ToLabel, labelizeFree)
import qualified WireCat.Label as Label
import Control.Monad.State.Strict (State, runState, state)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Row.Internal
  ( Extend,
    Forall,
    KnownSymbol,
    Label (..),
    Row,
    metamorph,
    toKey,
    type (.-),
  )
import Data.Row.Records (Rec)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Type.Reflection (Typeable, typeRep)

type NodeId = String

type AttrName = String

type TypeName = String

data Node = Node
  { nodeId :: NodeId,
    name :: String,
    input :: Map AttrName TypeName,
    output :: Map AttrName TypeName
  }
  deriving (Eq, Generic, Show)

instance FromJSON Node

instance ToJSON Node

data Plug = Plug
  { node :: NodeId,
    attr :: AttrName
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Plug

instance ToJSON Plug

data Edge = Edge
  { source :: Plug,
    target :: Plug
  }
  deriving (Eq, Generic, Show)

instance FromJSON Edge

instance ToJSON Edge

data Graph a b = Graph
  { nodes :: [Node],
    edges :: [Edge]
  }
  deriving (Eq, Generic, Show)

instance FromJSON (Graph a b)

instance ToJSON (Graph a b)

toGraph :: forall op r s. (ToLabel op) => Free op r s -> Graph (Rec r) (Rec s)
toGraph = fromFreeLabel . labelizeFree

fromFreeLabel :: Free Label.Label r s -> Graph (Rec r) (Rec s)
fromFreeLabel free =
  Graph
    { nodes = reverse compiledNodes,
      edges = reverse compiledEdges
    }
  where
    (_, (_, compiledNodes, compiledEdges)) =
      flip runState (0 :: Int, [], []) $
        build free []

data Wire = Wire
  { wireName :: AttrName,
    wireSource :: Plug
  }

type BuildState = (Int, [Node], [Edge])

build :: forall r s. Free Label.Label r s -> [Wire] -> State BuildState [Wire]
build IdentityR inputs = pure inputs
build (LiftR label) inputs =
  instantiateNode (primitiveNode label) inputs
build (ComposeR left right) inputs =
  build right inputs >>= build left
build ProjectR inputs =
  pure (filter (\w -> wireName w `Map.member` rowPortMap @s) inputs)
build (CombineR left right) inputs = do
  leftOutputs <- build left inputs
  rightOutputs <- build right inputs
  pure (leftOutputs ++ rightOutputs)
build (RelabelR old new) inputs =
  let oldStr = Text.unpack (toKey old)
      newStr = Text.unpack (toKey new)
      rename w
        | wireName w == oldStr = w {wireName = newStr}
        | otherwise = w
   in pure (map rename inputs)

instantiateNode :: (Int -> Node) -> [Wire] -> State BuildState [Wire]
instantiateNode mkNode inputs = do
  nodeDef <- freshNode mkNode
  addNode nodeDef
  addEdges (zipInputs nodeDef inputs)
  pure (outputPlugs nodeDef)

freshNode :: (Int -> Node) -> State BuildState Node
freshNode mkNode = state $ \(nextId, nodesAcc, edgesAcc) ->
  let nodeDef = mkNode nextId
   in (nodeDef, (nextId + 1, nodesAcc, edgesAcc))

addNode :: Node -> State BuildState ()
addNode nodeDef = state $ \(nextId, nodesAcc, edgesAcc) ->
  ((), (nextId, nodeDef : nodesAcc, edgesAcc))

addEdges :: [Edge] -> State BuildState ()
addEdges newEdges = state $ \(nextId, nodesAcc, edgesAcc) ->
  ((), (nextId, nodesAcc, reverse newEdges ++ edgesAcc))

primitiveNode ::
  forall r s.
  (Forall r Typeable, Forall s Typeable) =>
  Label.Label (Rec r) (Rec s) ->
  Int ->
  Node
primitiveNode label nextId =
  Node
    { nodeId = "n" ++ show nextId,
      name = Label.operationLabel label,
      input = rowPortMap @r,
      output = rowPortMap @s
    }

zipInputs :: Node -> [Wire] -> [Edge]
zipInputs nodeDef inputs =
  case traverse (`Map.lookup` inputsByName) (Map.keys (input nodeDef)) of
    Just matchedInputs -> zipByPosition matchedInputs
    Nothing -> zipByPosition inputs
  where
    inputsByName = Map.fromList [(wireName w, w) | w <- inputs]
    zipByPosition orderedInputs =
      zipWith
        (\w portName -> Edge (wireSource w) Plug {node = nodeId nodeDef, attr = portName})
        orderedInputs
        (Map.keys (input nodeDef))

outputPlugs :: Node -> [Wire]
outputPlugs nodeDef =
  [ Wire
      { wireName = portName,
        wireSource = Plug {node = nodeId nodeDef, attr = portName}
      }
    | portName <- Map.keys (output nodeDef)
  ]

rowPortMap :: forall r. (Forall r Typeable) => Map AttrName TypeName
rowPortMap = Map.fromList (rowPorts @r)

rowPorts :: forall r. (Forall r Typeable) => [(AttrName, TypeName)]
rowPorts = getRowPorts (metamorph @Type @r @Typeable @(,) @EmptyRow @RowPorts @FieldType proxy done field rebuild EmptyRow)
  where
    proxy = Proxy @(Proxy FieldType, Proxy (,))
    done EmptyRow = RowPorts []

    field ::
      forall label fieldType (rest :: Row Type).
      (Typeable fieldType) =>
      Label label ->
      EmptyRow rest ->
      (EmptyRow (rest .- label), FieldType fieldType)
    field _ EmptyRow = (EmptyRow, FieldType (show (typeRep @fieldType)))

    rebuild ::
      forall label fieldType (rest :: Row Type).
      (KnownSymbol label) =>
      Label label ->
      (RowPorts rest, FieldType fieldType) ->
      RowPorts (Extend label fieldType rest)
    rebuild label (RowPorts ports, FieldType fieldType) =
      RowPorts ((Text.unpack (toKey label), fieldType) : ports)

data EmptyRow (r :: Row Type) = EmptyRow

newtype RowPorts (r :: Row Type) = RowPorts {getRowPorts :: [(AttrName, TypeName)]}

newtype FieldType (a :: Type) = FieldType TypeName
