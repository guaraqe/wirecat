<p align="center">
  <img src="viewer/src/assets/wirecat.svg" alt="WireCat" width="420">
</p>

# wirecat

`wirecat` is a GHC plugin and small library for writing typed categorical
wiring diagrams with a restricted fragment of Haskell `proc` notation. A
single pipeline can be interpreted as executable code or rendered as a graph. Read the motivation for this project [here](https://guaraqe.com/posts/2026-05-24-why-cartesian-categories.html).

The notation currently covers the cartesian part of the language: composition,
projection, product-like combination, field relabeling, and primitive
operations. Case analysis is the next missing piece for the bicartesian side.

## Example

```haskell
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fplugin=WireCat #-}

import WireCat

[effect|
WordCount
  ReadPath    :: {} -> { path :: FilePath }
  LoadText    :: { path :: FilePath } -> { text :: String }
  CountWords  :: { text :: String } -> { words :: Int }
  CountLines  :: { text :: String } -> { lines :: Int }
  CountChars  :: { text :: String } -> { chars :: Int }
  WriteReport :: { path :: FilePath, words :: Int, lines :: Int, chars :: Int } -> {}
|]

wordCount :: WordCount :> cat => cat Empty Empty
wordCount = proc R {} -> do
  R {path} <- readPath -< R {}
  R {text} <- loadText -< R {path}
  R {words} <- countWords -< R {text}
  R {lines} <- countLines -< R {text}
  R {chars} <- countChars -< R {text}
  writeReport -< R {path, words, lines, chars}
```

That same definition can be run with a concrete interpreter, captured as a free
categorical term, or exported as DOT, SVG, and JSON for the React viewer:

<p align="center">
  <img src="diagram.png" alt="WireCat" width="420">
</p>

## Commands

```sh
cabal build all
cabal test all
cabal run examples -- word-count graph
just viewer-dev
```

Generated example graphs are written to `tmp/`.
