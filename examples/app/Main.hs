{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fplugin=WireCat #-}

module Main (main) where

import WireCat
import Control.Monad (void)
import Data.Aeson (toJSON)
import qualified Data.Map.Strict as Map
import Data.Row.Internal (Empty)
import Data.Row.Records hiding (compose)
import Options.Applicative
  ( Parser,
    (<**>),
    command,
    execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    subparser,
  )
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))

-- File copy pipeline ---------------------------------------------------------

data FileCopy a b where
  ReadSrc ::
    FileCopy
      (Rec Empty)
      (Rec ("filepath" .== FilePath))
  ReadDst ::
    FileCopy
      (Rec ("src" .== FilePath))
      (Rec ("dst" .== FilePath))
  CopyFile ::
    FileCopy
      (Rec (("src" .== FilePath) .// ("dst" .== FilePath)))
      (Rec Empty)

deriving instance Show (FileCopy a b)

instance ToLabel FileCopy

instance Interpret (KleisliRec IO) FileCopy where
  interpret ReadSrc = KleisliRec $ \_ -> do
    putStr "Source path: "
    src <- getLine
    pure (#filepath .== src)
  interpret ReadDst = KleisliRec $ \_ -> do
    putStr "Destination path: "
    dst <- getLine
    pure (#dst .== dst)
  interpret CopyFile = KleisliRec $ \r -> do
    copyFile (r .! #src) (r .! #dst)
    pure empty

fileCopy :: FileCopy :> cat => cat Empty Empty
fileCopy = proc R {} -> do
  R {filepath} <- interpret ReadSrc -< R {}
  R {dst} <- interpret ReadDst -< R {src = filepath}
  interpret CopyFile -< R {src = filepath, dst}

-- Word count pipeline --------------------------------------------------------

data WordCount a b where
  ReadPath ::
    WordCount
      (Rec Empty)
      (Rec ("path" .== FilePath))
  LoadText ::
    WordCount
      (Rec ("path" .== FilePath))
      (Rec ("text" .== String))
  CountWords ::
    WordCount
      (Rec ("text" .== String))
      (Rec ("words" .== Int))
  CountLines ::
    WordCount
      (Rec ("text" .== String))
      (Rec ("lines" .== Int))
  CountChars ::
    WordCount
      (Rec ("text" .== String))
      (Rec ("chars" .== Int))
  WriteReport ::
    WordCount
      ( Rec
          ( ("path" .== FilePath)
              .// ("words" .== Int)
              .// ("lines" .== Int)
              .// ("chars" .== Int)
          )
      )
      (Rec Empty)

deriving instance Show (WordCount a b)

instance ToLabel WordCount

instance Interpret (KleisliRec IO) WordCount where
  interpret ReadPath = KleisliRec $ \_ -> do
    putStr "Path: "
    p <- getLine
    pure (#path .== p)
  interpret LoadText = KleisliRec $ \r -> do
    t <- readFile (r .! #path)
    pure (#text .== t)
  interpret CountWords = KleisliRec $ \r ->
    pure (#words .== length (words (r .! #text)))
  interpret CountLines = KleisliRec $ \r ->
    pure (#lines .== length (lines (r .! #text)))
  interpret CountChars = KleisliRec $ \r ->
    pure (#chars .== length (r .! #text))
  interpret WriteReport = KleisliRec $ \r -> do
    let out = (r .! #path) ++ ".wc"
        body =
          unlines
            [ "words: " ++ show (r .! #words),
              "lines: " ++ show (r .! #lines),
              "chars: " ++ show (r .! #chars)
            ]
    writeFile out body
    putStrLn $ "Wrote " ++ out
    pure empty

wordCount :: WordCount :> cat => cat Empty Empty
wordCount = proc R {} -> do
  R {path} <- interpret ReadPath -< R {}
  R {text} <- interpret LoadText -< R {path}
  R {words} <- interpret CountWords -< R {text}
  R {lines} <- interpret CountLines -< R {text}
  R {chars} <- interpret CountChars -< R {text}
  interpret WriteReport -< R {path, words, lines, chars}

-- CLI ------------------------------------------------------------------------

data Program = FileCopyP | WordCountP

data Action = GraphCmd | RunCmd

data Opts = Opts Program Action

programName :: Program -> String
programName FileCopyP = "file-copy"
programName WordCountP = "word-count"

optsParser :: Parser Opts
optsParser =
  subparser
    ( command
        "file-copy"
        (info (Opts FileCopyP <$> actionParser) (progDesc "File copy pipeline"))
        <> command
          "word-count"
          (info (Opts WordCountP <$> actionParser) (progDesc "Word count report pipeline"))
    )

actionParser :: Parser Action
actionParser =
  subparser
    ( command "graph" (info (pure GraphCmd) (progDesc "Render the pipeline graph"))
        <> command "run" (info (pure RunCmd) (progDesc "Run the pipeline"))
    )

renderGraph :: Program -> IO ()
renderGraph prog = do
  let name = programName prog
      tmpDir = "tmp"
      dotFile = tmpDir </> (name ++ ".dot")
      svgFile = tmpDir </> (name ++ ".svg")
      jsonFile = tmpDir </> (name ++ ".json")
  createDirectoryIfMissing True tmpDir
  case prog of
    FileCopyP -> do
      let g = toGraph @FileCopy fileCopy
      writeDotFile dotFile g
      _ <- writeSvgFile svgFile g
      writeJsonFile jsonFile (Map.singleton "main" (toJSON g))
    WordCountP -> do
      let g = toGraph @WordCount wordCount
      writeDotFile dotFile g
      _ <- writeSvgFile svgFile g
      writeJsonFile jsonFile (Map.singleton "main" (toJSON g))
  putStrLn $ "Wrote " ++ dotFile
  putStrLn $ "Wrote " ++ svgFile
  putStrLn $ "Wrote " ++ jsonFile

runPipeline :: Program -> IO ()
runPipeline FileCopyP = void $ runKleisliRec fileCopy empty
runPipeline WordCountP = void $ runKleisliRec wordCount empty

main :: IO ()
main = do
  Opts prog act <-
    execParser
      ( info
          (optsParser <**> helper)
          (fullDesc <> progDesc "wirecat examples")
      )
  case act of
    GraphCmd -> renderGraph prog
    RunCmd -> runPipeline prog
