{-# LANGUAGE GADTs #-}

module Tutorial.Mermaid
  ( Node (..),
    Edge (..),
    Mermaid (..),
    renderMermaid,
    writeMermaidFile,
    renderMermaidWithMmdc,
  )
where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Process (callProcess)

data Node = Node
  { nodeId :: String,
    label :: String
  }

data Edge = Edge
  { fromNode :: String,
    toNode :: String,
    edgeLabel :: Maybe String
  }

data Mermaid = Mermaid
  { mermaidNodes :: [Node],
    mermaidEdges :: [Edge]
  }

renderMermaid :: Mermaid -> String
renderMermaid (Mermaid nodes edges) =
  unlines $
    ["flowchart TD"]
      ++ map renderNode nodes
      ++ map renderEdge edges
  where
    renderNode (Node i l) =
      "  " ++ i ++ "[\"" ++ escape l ++ "\"]"

    renderEdge (Edge a b Nothing) =
      "  " ++ a ++ " --> " ++ b
    renderEdge (Edge a b (Just l)) =
      "  " ++ a ++ " -- \"" ++ escape l ++ "\" --> " ++ b

    escape = concatMap go
    go '"' = "\\\""
    go c = [c]

writeMermaidFile :: FilePath -> Mermaid -> IO ()
writeMermaidFile path = writeFile path . renderMermaid

renderMermaidWithMmdc :: String -> Mermaid -> IO ()
renderMermaidWithMmdc basename mermaid = do
  createDirectoryIfMissing True (takeDirectory basename)
  let inputPath = basename <> ".mmd"
      outputPath = basename <> ".svg"
  writeMermaidFile inputPath mermaid
  callProcess "mmdc" ["-i", inputPath, "-o", outputPath]
