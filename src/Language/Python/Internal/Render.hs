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

renderCommaSep :: (a -> String) -> CommaSep a -> String
renderCommaSep _ CommaSepNone = mempty
renderCommaSep f (CommaSepOne a c) = f a <> foldMap ((<> ",") . foldMap renderWhitespace) c
renderCommaSep f (CommaSepMany a ws1 ws2 c) =
  f a <>
  foldMap renderWhitespace ws1 <> "," <> foldMap renderWhitespace ws2 <>
  renderCommaSep f c

renderExpr :: Expr v a -> String
renderExpr (Parens _ ws1 e ws2) =
  "(" <> foldMap renderWhitespace ws1 <>
  renderExpr e <>
  foldMap renderWhitespace ws2 <> ")"
renderExpr (Bool _ b) = show b
renderExpr (Negate _ ws expr) =
  "-" <> foldMap renderWhitespace ws <>
    case expr of
      BinOp _ _ _ Exp{} _ _ -> renderExpr expr
      BinOp{} -> "(" <> renderExpr expr <> ")"
      _ -> renderExpr expr
renderExpr (Int _ n) = show n
renderExpr (Ident _ name) = name
renderExpr (List _ ws1 exprs ws2) =
  "[" <> foldMap renderWhitespace ws1 <>
  renderCommaSep renderExpr exprs <>
  foldMap renderWhitespace ws2 <> "]"
renderExpr (Call _ expr ws args) =
  renderExpr expr <>
  foldMap renderWhitespace ws <>
  renderArgs args
renderExpr (Deref _ expr ws1 ws2 name) =
  (case expr of
    Int{} -> "(" <> renderExpr expr <> ")"
    _ -> renderExpr expr) <>
  foldMap renderWhitespace ws1 <> "." <> foldMap renderWhitespace ws2 <>
  name
renderExpr (None _) = "None"
renderExpr (BinOp _ e1 ws1 op ws2 e2) =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case e1 of
        BinOp _ _ _ lOp _ _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    rEntry =
      case e2 of
        BinOp _ _ _ rOp _ _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    (e1f, e2f) =
      case entry ^. opAssoc of
        L | Just L <- rEntry ^? _Just.opAssoc -> (Nothing, Just bracket)
        R | Just R <- lEntry ^? _Just.opAssoc -> (Just bracket, Nothing)
        _ -> (Nothing, Nothing)

    e1f' =
      case (e1, op) of
        (Negate{}, Exp{}) -> Just bracket
        _ -> do
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
    foldMap renderWhitespace ws1  <>
    renderBinOp op <>
    foldMap renderWhitespace ws2 <>
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
renderBinOp (Plus _) = "+"
renderBinOp (Minus _) = "-"
renderBinOp (Multiply _) = "*"
renderBinOp (Divide _) = "/"
renderBinOp (Exp _) = "**"
renderBinOp (BoolAnd _) = "and"
renderBinOp (BoolOr _) = "or"
