module GHC.Compat.Expr
  ( HsExpr (..),
    LHsExpr,
    MatchGroup (..),
    Match (..),
    GRHSs (..),
    GRHS (..),
    HsLocalBindsLR (..),
    LPat,
    Pat (..),
    HsCmdTop (..),
    HsCmd (..),
    LHsCmd,
    CmdLStmt,
    StmtLR (..),
    HsArrAppType (..),
    HsTupArg (..),
    HsRecFields (..),
    HsFieldBind (..),
    GhcRn,
    GhcPs,
    Located,
    GenLocated (..),
    SrcSpan (..),
    noSrcSpan,
    noExtField,
    hsVarPs,
    hsAppsPs,
    hsParPs,
    hsOverLabelPs,
    rdrNameOcc,
  )
where

import qualified GHC.Compat.All as GHC
import GHC.Hs
import qualified GHC.Plugins as Plugins
import GHC.Types.Name.Reader (RdrName)
import qualified GHC.Types.Name.Reader as RdrName
import GHC.Types.SourceText (SourceText (NoSourceText))
import GHC.Types.SrcLoc

hsVarPs :: SrcSpan -> RdrName -> LHsExpr GhcPs
hsVarPs l n = L (noAnnSrcSpan l) (HsVar noExtField (L (noAnnSrcSpan l) n))

hsAppsPs :: SrcSpan -> LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
hsAppsPs l = foldl' (\f x -> L (noAnnSrcSpan l) (HsApp noExtField f x))

hsParPs :: SrcSpan -> LHsExpr GhcPs -> LHsExpr GhcPs
hsParPs l e = L (noAnnSrcSpan l) (HsPar (EpTok noAnn, EpTok noAnn) e)

hsOverLabelPs :: SrcSpan -> String -> LHsExpr GhcPs
hsOverLabelPs l s =
  L (noAnnSrcSpan l) (HsOverLabel NoSourceText (Plugins.mkFastString s))

rdrNameOcc :: String -> RdrName
rdrNameOcc = RdrName.mkRdrUnqual . GHC.mkVarOcc
