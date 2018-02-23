module Language.Python.Internal.Render where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Prism
import Control.Lens.Wrapped
import Data.List
import Data.Maybe
import Data.Monoid
import Language.Python.Internal.Syntax

renderWhitespace :: Whitespace -> String
renderWhitespace Space = " "
renderWhitespace Tab = "\t"
renderWhitespace (Continued ws) = "\\\n" <> foldMap renderWhitespace ws

renderExpr :: Expr v a -> String
renderExpr (Int _ n) = show n
renderExpr (Ident _ name) = name
renderExpr (List _ exprs) = "[" <> intercalate ", " (fmap renderExpr exprs) <> "]"
renderExpr (Call _ expr args) = renderExpr expr <> renderArgs args
renderExpr (Deref _ expr name) =
  (case expr of
    Int{} -> "(" <> renderExpr expr <> ")"
    _ -> renderExpr expr) <>
  "." <> name
renderExpr (None _) = "None"
renderExpr (BinOp _ op e1 e2) =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case e1 of
        BinOp _ lOp _ _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    rEntry =
      case e2 of
        BinOp _ rOp _ _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    (e1f, e2f) =
      case entry ^. opAssoc of
        L | Just L <- rEntry ^? _Just.opAssoc -> (Nothing, Just bracket)
        R | Just R <- lEntry ^? _Just.opAssoc -> (Just bracket, Nothing)
        _ -> (Nothing, Nothing)

    e1f' = do
      p <- lEntry ^? _Just.opPrec
      if p < entry ^. opPrec
      then Just bracket
      else Nothing

    e2f' = do
      p <- rEntry ^? _Just.opPrec
      if p < entry ^. opPrec
      then Just bracket
      else Nothing
  in
    fromMaybe id (e1f <|> e1f') (renderExpr e1) <>
    " " <>
    renderBinOp op <>
    " " <>
    fromMaybe id (e2f <|> e2f') (renderExpr e2)
  where
    bracket a = "(" <> a <> ")"

renderStatement :: Statement v a -> [String]
renderStatement (Fundef _ name params body) =
  ("def " <> name <> renderParams params <> ":") :
  (view _Wrapped body >>= \(_, a, b) -> (foldMap renderWhitespace a <>) <$> renderStatement b)
renderStatement (Return _ expr) = ["return " <> renderExpr expr]
renderStatement (Expr _ expr) = [renderExpr expr]
renderStatement (If _ expr body) =
  ("if " <> renderExpr expr <> ":") :
  (view _Wrapped body >>= \(_, a, b) -> (foldMap renderWhitespace a <>) <$> renderStatement b)
renderStatement (Assign _ lvalue rvalue) = [renderExpr lvalue <> " = " <> renderExpr rvalue]
renderStatement (Pass _) = ["pass"]

renderArgs :: Args v a -> String
renderArgs a = "(" <> go a <> ")"
  where
    go (NoArgs _) = ""
    go (PositionalArg _ expr args) =
      renderExpr expr <>
      case args of
        NoArgs _ -> ""
        _ ->
          ", " <>
          go args

renderParams :: Params v a -> String
renderParams a = "(" <> intercalate ", " (fmap go a) <> ")"
  where
    go (PositionalParam _ name) = name
    go (KeywordParam _ name expr) = name <> "=" <> renderExpr expr

renderBinOp :: BinOp a -> String
renderBinOp (Is _) = "is"
renderBinOp (Minus _) = "-"
