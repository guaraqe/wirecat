{-# LANGUAGE OverloadedStrings #-}

module WireCat.Dot
  ( DotOptions (..),
    defaultDotOptions,
    toDotGraph,
    toDotGraphWith,
    renderDot,
    renderDotWith,
    writeDotFile,
    writeDotFileWith,
    writeSvgFile,
    writeSvgFileWith,
  )
where

import Data.Char (isAlphaNum)
import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes.Complete as DotAttr
import qualified Data.GraphViz.Commands.IO as GraphVizIO
import qualified Data.GraphViz.Types.Generalised as DotGraph
import qualified Data.GraphViz.Types.Monadic as Dot
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as Text
import WireCat.Graph (AttrName, Edge (..), Graph (..), Node (..), NodeId, Plug (..), TypeName)

data DotOptions = DotOptions
  { graphName :: Text.Text,
    rankDir :: DotAttr.RankDir
  }

defaultDotOptions :: DotOptions
defaultDotOptions =
  DotOptions
    { graphName = "wirecat",
      rankDir = DotAttr.FromTop
    }

toDotGraph :: Graph a b -> DotGraph.DotGraph String
toDotGraph = toDotGraphWith defaultDotOptions

toDotGraphWith :: DotOptions -> Graph a b -> DotGraph.DotGraph String
toDotGraphWith opts graph =
  Dot.digraph (DotGraph.Str (graphName opts)) $ do
    Dot.graphAttrs [DotAttr.RankDir (rankDir opts)]
    mapM_ (renderNode opts) (nodes graph)
    mapM_ (renderEdge graph) (edges graph)

renderDot :: Graph a b -> String
renderDot = renderDotWith defaultDotOptions

renderDotWith :: DotOptions -> Graph a b -> String
renderDotWith opts = Text.unpack . GraphViz.printDotGraph . toDotGraphWith opts

writeDotFile :: FilePath -> Graph a b -> IO ()
writeDotFile = writeDotFileWith defaultDotOptions

writeDotFileWith :: DotOptions -> FilePath -> Graph a b -> IO ()
writeDotFileWith opts path = GraphVizIO.writeDotFile path . toDotGraphWith opts

writeSvgFile :: FilePath -> Graph a b -> IO FilePath
writeSvgFile = writeSvgFileWith defaultDotOptions

writeSvgFileWith :: DotOptions -> FilePath -> Graph a b -> IO FilePath
writeSvgFileWith opts path graph =
  GraphViz.runGraphvizCommand GraphViz.Dot (toDotGraphWith opts graph) GraphViz.Svg path

renderNode :: DotOptions -> Node -> Dot.Dot String
renderNode _ nodeDef =
  Dot.node (nodeId nodeDef) [DotAttr.Shape DotAttr.MRecord, DotAttr.Label (nodeRecordLabel nodeDef)]

nodeRecordLabel :: Node -> DotAttr.Label
nodeRecordLabel nodeDef =
  DotAttr.RecordLabel
    [ DotAttr.FlipFields
        [ portRow InputPort (Map.toAscList (input nodeDef)),
          DotAttr.FieldLabel (Text.pack (name nodeDef)),
          portRow OutputPort (Map.toAscList (output nodeDef))
        ]
    ]

renderEdge :: Graph a b -> Edge -> Dot.Dot String
renderEdge graph edgeDef =
  Dot.edge
    (node (source edgeDef))
    (node (target edgeDef))
    ( [ DotAttr.TailPort (portPos OutputPort (attr src)),
        DotAttr.HeadPort (portPos InputPort (attr dst))
      ]
        ++ edgeLabel graph src
    )
  where
    src = source edgeDef
    dst = target edgeDef

portRow :: PortDirection -> [(AttrName, String)] -> DotAttr.RecordField
portRow _ [] = DotAttr.FieldLabel ""
portRow direction attrs =
  DotAttr.FlipFields (map (uncurry (portField direction)) attrs)

portField :: PortDirection -> AttrName -> String -> DotAttr.RecordField
portField direction attrName _typeName =
  DotAttr.LabelledTarget
    (portName direction attrName)
    (Text.pack attrName)

edgeLabel :: Graph a b -> Plug -> [DotAttr.Attribute]
edgeLabel graph plug =
  case sourceType graph plug of
    Just typeName | not (null typeName) -> [DotAttr.XLabel (DotAttr.StrLabel (Text.pack typeName))]
    _ -> []

sourceType :: Graph a b -> Plug -> Maybe TypeName
sourceType graph plug = do
  nodeDef <- lookupNode graph (node plug)
  Map.lookup (attr plug) (output nodeDef)

lookupNode :: Graph a b -> NodeId -> Maybe Node
lookupNode graph targetNode =
  findNode (nodes graph)
  where
    findNode [] = Nothing
    findNode (nodeDef : rest)
      | nodeId nodeDef == targetNode = Just nodeDef
      | otherwise = findNode rest

portName :: PortDirection -> AttrName -> DotAttr.PortName
portName direction attrName =
  DotAttr.PN (Text.pack (prefix ++ sanitize attrName))
  where
    prefix = case direction of
      InputPort -> "in_"
      OutputPort -> "out_"

portPos :: PortDirection -> AttrName -> DotAttr.PortPos
portPos direction attrName =
  DotAttr.LabelledPort
    (portName direction attrName)
    (Just compass)
  where
    compass = case direction of
      InputPort -> DotAttr.North
      OutputPort -> DotAttr.South

sanitize :: String -> String
sanitize = map replace
  where
    replace c
      | isAlphaNum c = c
      | otherwise = '_'

data PortDirection
  = InputPort
  | OutputPort
