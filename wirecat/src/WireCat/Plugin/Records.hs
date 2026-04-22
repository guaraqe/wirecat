{-# LANGUAGE LambdaCase #-}

module WireCat.Plugin.Records (transformRecords) where

import qualified Data.Set as Set
import GHC.Compat.Expr
import GHC.Hs
import qualified GHC.Plugins as Plugins
import qualified GHC.Types.Name.Reader as RdrName
import GHC.Types.SrcLoc (unLoc)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

-- | Record-category morphism IR.
data Morph
  = MId
  | MCompose Morph Morph
  | MProject
  | MCombine Morph Morph
  | MRelabel String String
  | MPickField String
  | MTerm (LHsExpr GhcPs)

composeM :: Morph -> Morph -> Morph
composeM MId g = g
composeM f MId = f
composeM f g = MCompose f g

-- | Entry point: rewrite any @proc ... -> ...@ expression; leave others alone.
transformRecords :: LHsExpr GhcPs -> LHsExpr GhcPs
transformRecords (L loc (HsProc _ pat (L _ (HsCmdTop _ cmd)))) =
  case compileProc pat cmd of
    Right morph -> morphToExpr (locA loc) morph
    Left err -> error ("WireCat.Plugin.Records: " ++ err)
transformRecords e = e

ppLoc :: SrcSpan -> String
ppLoc = showSDocUnsafe . ppr

atLoc :: SrcSpan -> String -> String
atLoc l msg = ppLoc l ++ ": " ++ msg

-- | Compile a proc expression to a Morph.
compileProc :: LPat GhcPs -> LHsCmd GhcPs -> Either String Morph
compileProc pat cmd = do
  ctx <- parsePatFields "proc" pat
  compileCmd ctx cmd

-- | Compile a command (either @do ...@ or a single @op -< rhs@).
compileCmd :: [String] -> LHsCmd GhcPs -> Either String Morph
compileCmd ctx (L _ (HsCmdDo _ (L _ stmts))) =
  compileStmts ctx stmts
compileCmd ctx (L _ (HsCmdArrApp _ opExpr inputExpr HsFirstOrderApp _)) = do
  rhs <- buildRhs ctx inputExpr
  pure (composeM (MTerm opExpr) rhs)
compileCmd _ (L lloc cmd) =
  Left $
    atLoc (locA lloc) $
      "unsupported command:\n"
        ++ showSDocUnsafe (ppr cmd)

-- | Compile a non-empty list of do statements under a given context.
compileStmts :: [String] -> [CmdLStmt GhcPs] -> Either String Morph
compileStmts _ [] = Left "empty proc-do block"
compileStmts ctx [L _ (LastStmt _ cmd _ _)] =
  compileCmd ctx cmd
compileStmts ctx (L _ (BindStmt _ pat cmd) : rest) = do
  opMorph <- compileCmd ctx cmd
  (newNames, outRelabels) <- parseLhs pat
  let stepMorph = composeM outRelabels opMorph
      extMorph = MCombine MId stepMorph
      ctx' = ctx ++ newNames
  continuation <- compileStmts ctx' rest
  pure (composeM continuation extMorph)
compileStmts ctx [L _ (BodyStmt _ cmd _ _)] =
  compileCmd ctx cmd
compileStmts _ (L lloc stmt : _) =
  Left $
    atLoc (locA lloc) $
      "unsupported statement in proc-do:\n" ++ showSDocUnsafe (ppr stmt)

-- | Parse the proc's top-level input pattern (only supports record pattern).
parsePatFields :: String -> LPat GhcPs -> Either String [String]
parsePatFields ctx lpat = case unLoc (stripParPat lpat) of
  ConPat _ _ (RecCon flds) -> mapM (fieldBinder ctx) (rec_flds flds)
  WildPat {} -> pure []
  _ ->
    Left $
      atLoc (getLocA lpat) $
        "expected record pattern like R {..} in "
          ++ ctx
          ++ ", got:\n"
          ++ showSDocUnsafe (ppr (unLoc lpat))

stripParPat :: LPat GhcPs -> LPat GhcPs
stripParPat (L _ (ParPat _ inner)) = stripParPat inner
stripParPat p = p

-- | Each record field pattern binds a name: @R {x}@ (pun) or @R {y = w}@ (rebind).
fieldBinder ::
  String ->
  LHsFieldBind GhcPs (LFieldOcc GhcPs) (LPat GhcPs) ->
  Either String String
fieldBinder ctx (L lloc (HsFieldBind _ (L _ fieldOcc) rhsPat pun))
  | pun = pure (occStr (rdrNameFieldOcc fieldOcc))
  | otherwise = case unLoc rhsPat of
      VarPat _ (L _ name) -> pure (occStr name)
      _ ->
        Left $
          atLoc (locA lloc) $
            "field rhs must be a variable pattern in " ++ ctx

-- | Parse the LHS of @<-@ and return:
--   * the list of names added to the context, in field order
--   * a Morph renaming the op's output row to those names
parseLhs :: LPat GhcPs -> Either String ([String], Morph)
parseLhs lpat = case unLoc (stripParPat lpat) of
  ConPat _ _ (RecCon flds) -> do
    entries <- mapM fieldEntry (rec_flds flds)
    let newNames = map snd entries
        relabels = foldr composeM MId [MRelabel o n | (o, n) <- entries, o /= n]
    pure (newNames, relabels)
  WildPat {} -> pure ([], MId)
  _ ->
    Left $
      atLoc (getLocA lpat) $
        "expected record pattern on LHS of <-:\n"
          ++ showSDocUnsafe (ppr (unLoc lpat))
  where
    fieldEntry ::
      LHsFieldBind GhcPs (LFieldOcc GhcPs) (LPat GhcPs) ->
      Either String (String, String)
    fieldEntry (L lloc (HsFieldBind _ (L _ fieldOcc) rhsPat pun)) = do
      let fieldName = occStr (rdrNameFieldOcc fieldOcc)
      binder <-
        if pun
          then pure fieldName
          else case unLoc rhsPat of
            VarPat _ (L _ v) -> pure (occStr v)
            _ ->
              Left $
                atLoc (locA lloc) "field rhs must be a variable pattern on LHS of <-"
      pure (fieldName, binder)

-- | Build a morphism @cat ctx opInput@ from an RHS expression like @R {a, k = v}@.
buildRhs :: [String] -> LHsExpr GhcPs -> Either String Morph
buildRhs ctx lexpr = case unLoc (stripParExpr lexpr) of
  RecordCon _ _ flds -> do
    entries <- mapM rhsField (rec_flds flds)
    case entries of
      [] -> pure MProject -- project to empty row
      es -> pure (foldr1 MCombine (map fieldMorph es))
  _ ->
    Left $
      atLoc (getLocA lexpr) $
        "expected record expression on RHS of -<:\n"
          ++ showSDocUnsafe (ppr (unLoc lexpr))
  where
    ctxSet = Set.fromList ctx
    rhsField ::
      LHsFieldBind GhcPs (LFieldOcc GhcPs) (LHsExpr GhcPs) ->
      Either String (String, String)
    rhsField (L lloc (HsFieldBind _ (L _ fieldOcc) valExpr pun)) = do
      let target = occStr (rdrNameFieldOcc fieldOcc)
          loc = locA lloc
      source <-
        if pun
          then pure target
          else case unLoc (stripParExpr valExpr) of
            HsVar _ (L _ v) -> pure (occStr v)
            _ -> Left (atLoc loc "field value on RHS must be a variable")
      if Set.member source ctxSet
        then pure (target, source)
        else
          Left $
            atLoc loc $
              "unbound field on RHS: "
                ++ source
                ++ "\n  in scope: "
                ++ show ctx

    fieldMorph (target, source)
      | source == target = MPickField source
      | otherwise = composeM (MRelabel source target) (MPickField source)

stripParExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
stripParExpr (L _ (HsPar _ inner)) = stripParExpr inner
stripParExpr e = e

occStr :: Plugins.RdrName -> String
occStr = Plugins.occNameString . RdrName.rdrNameOcc

rdrNameFieldOcc :: FieldOcc GhcPs -> Plugins.RdrName
rdrNameFieldOcc (FieldOcc _ (L _ n)) = n

-- | Convert a Morph into a GhcPs expression.
morphToExpr :: SrcSpan -> Morph -> LHsExpr GhcPs
morphToExpr l = go
  where
    go MId = hsVarPs l (rdrNameOccStr "identity")
    go (MCompose f g) =
      hsParPs l $
        hsAppsPs l (hsVarPs l (rdrNameOccStr "compose")) [go f, go g]
    go MProject = hsVarPs l (rdrNameOccStr "project")
    go (MCombine f g) =
      hsParPs l $
        hsAppsPs l (hsVarPs l (rdrNameOccStr "combine")) [go f, go g]
    go (MRelabel old new) =
      hsParPs l $
        hsAppsPs
          l
          (hsVarPs l (rdrNameOccStr "relabel"))
          [hsOverLabelPs l old, hsOverLabelPs l new]
    go (MPickField name) =
      hsParPs l $
        hsAppsPs
          l
          (hsVarPs l (rdrNameOccStr "pickField"))
          [hsOverLabelPs l name]
    go (MTerm e) = e

rdrNameOccStr :: String -> Plugins.RdrName
rdrNameOccStr = rdrNameOcc
