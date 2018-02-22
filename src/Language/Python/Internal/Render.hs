module Language.Python.Internal.Render where

import Control.Lens (view, _Wrapped)
import Data.List
import Data.Monoid
import Language.Python.Internal.Syntax

renderWhitespace :: Whitespace -> String
renderWhitespace Space = " "
renderWhitespace Tab = "\t"
renderWhitespace (Continued ws) = "\\\n" <> foldMap renderWhitespace ws

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

renderCompOp :: CompOp a -> String
renderCompOp (Is _) = "is"
