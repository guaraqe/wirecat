module GHC.Compat.All
  ( DynFlags,
    HscEnv,
    TcM,
    ModuleName,
    Name,
    FindResult (..),
    PkgQual (..),
    Boxity (..),
    Origin (..),
    tupleDataCon,
    getName,
    defaultFixity,
    findImportedModule,
    lookupOrig,
    mkModuleName,
    moduleNameString,
    mkVarOcc,
    mkDataOcc,
    occName,
    occNameString,
    ppr,
    showSDocUnsafe,
    text,
    (<+>),
  )
where

import GHC.Builtin.Types (tupleDataCon)
import GHC.Driver.Env.Types (HscEnv)
import GHC.Driver.Session (DynFlags)
import GHC.Iface.Env (lookupOrig)
import GHC.Tc.Utils.Monad (TcM)
import GHC.Types.Basic (Boxity (..), Origin (..))
import GHC.Types.Fixity (defaultFixity)
import GHC.Types.Name (Name, getName)
import GHC.Types.Name.Occurrence (mkDataOcc, mkVarOcc, occName, occNameString)
import GHC.Types.PkgQual (PkgQual (..))
import GHC.Unit.Finder (FindResult (..), findImportedModule)
import GHC.Unit.Module (ModuleName, mkModuleName, moduleNameString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe, text, (<+>))
