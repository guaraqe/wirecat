{-# LANGUAGE GADTs #-}

module Tutorial.Category
  ( Free (..),
    compose,
    inject,
    toMermaid,
  )
where

import Tutorial.Label
import Tutorial.Mermaid (Edge (..), Mermaid (..), Node (..))
import WireCat.Free
import WireCat.Records (compose)

--------------------------------------------------------------------------------
-- Mermaid

labelize :: (ToLabel op) => Free op a b -> Free Label a b
labelize IdentityR = IdentityR
labelize (LiftR op) = LiftR (toLabel op)
labelize (ComposeR left right) = ComposeR (labelize left) (labelize right)
labelize ProjectR = ProjectR
labelize (CombineR left right) = CombineR (labelize left) (labelize right)
labelize (RelabelR old new) = RelabelR old new

toMermaid :: (ToLabel op) => Free op a b -> Mermaid
toMermaid = linearMermaid . reverse . go . labelize
  where
    startNodeId = "boundaryStart"
    endNodeId = "boundaryEnd"

    go :: Free Label x y -> [(String, String, String)]
    go IdentityR = []
    go (LiftR label) =
      [(inputLabel label, operationLabel label, outputLabel label)]
    go (ComposeR left right) = go left ++ go right
    go ProjectR = [("", "project", "")]
    go (CombineR left right) = go left ++ go right ++ [("", "combine", "")]
    go (RelabelR _ _) = [("", "relabel", "")]

    linearMermaid :: [(String, String, String)] -> Mermaid
    linearMermaid [] =
      Mermaid
        [Node startNodeId "Input", Node endNodeId "Output"]
        [Edge startNodeId endNodeId Nothing]
    linearMermaid labels = Mermaid nodes edges
      where
        (initialObjectLabel, _, _) : _ = labels
        nodes =
          [ Node startNodeId "Input"
          ]
            ++ [ Node (operationNodeName i) operationText
               | (i, (_, operationText, _)) <- zip [0 :: Int ..] labels
               ]
            ++ [ Node endNodeId "Output"
               ]
        objectLabels =
          initialObjectLabel : [outputText | (_, _, outputText) <- labels]
        edges =
          [ Edge (fromNode i) (toNode i) (Just objectLabel)
          | (i, objectLabel) <- zip [0 :: Int ..] objectLabels
          ]
        operationNodeName i = "op" ++ show i

        edgeEndpoints i =
          case i of
            0 -> (startNodeId, operationNodeName (0 :: Int))
            _ | i == length labels -> (operationNodeName (i - 1), endNodeId)
            _ -> (operationNodeName (i - 1), operationNodeName i)

        fromNode i = fst (edgeEndpoints i)
        toNode i = snd (edgeEndpoints i)
