module Language.Python.Internal.Render where

import Data.List
import Data.Monoid
import Language.Python.Internal.Syntax

renderExpr :: Expr v a -> String
renderExpr (Ident _ name) = name
renderExpr (List _ exprs) = "[" <> intercalate ", " (fmap renderExpr exprs) <> "]"
renderExpr (Call _ expr args) = renderExpr expr <> renderArgs args
renderExpr (Deref _ expr name) = renderExpr expr <> "." <> name
renderExpr (None _) = "None"
renderExpr (Comp _ op e1 e2) =
  renderExpr e1 <>
  " " <>
  renderCompOp op <>
  " " <>
  renderExpr e2

renderStatement :: Statement v a -> [String]
renderStatement (Fundef _ name params body) =
  ("def " <> name <> renderParams params <> ":") :
  (body >>= fmap ("    "<>) . renderStatement)
renderStatement (Return _ expr) = ["return " <> renderExpr expr]
renderStatement (Expr _ expr) = [renderExpr expr]
renderStatement (If _ expr body) =
  ("if " <> renderExpr expr <> ":") :
  (body >>= fmap ("    " <>) . renderStatement)
renderStatement (Assign _ lvalue rvalue) = [renderExpr lvalue <> " = " <> renderExpr rvalue]

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

renderCompOp :: CompOp a -> String
renderCompOp (Is a) = "is"
