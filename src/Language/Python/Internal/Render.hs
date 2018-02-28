{-# language GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Render where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Prism
import Control.Lens.Wrapped
import Data.Foldable
import Data.Maybe
import Data.Semigroup (Semigroup(..))
import Language.Python.Internal.Syntax

data Lines a
  = NoLines
  | OneLine a
  | ManyLines a Newline (Lines a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
listToLines :: Newline -> [a] -> Lines a
listToLines _ [] = NoLines
listToLines _ [a] = OneLine a
listToLines nl (a:as) = ManyLines a nl $ listToLines nl as

endWith :: Newline -> Lines a -> Lines a
endWith nl NoLines = NoLines
endWith nl (OneLine a) = ManyLines a nl NoLines
endWith nl (ManyLines a nl' as) =
  ManyLines
    a
    (case as of; NoLines -> nl; _ -> nl')
    (case as of; NoLines -> NoLines; _ -> endWith nl as)

renderLines :: Lines String -> String
renderLines NoLines = ""
renderLines (OneLine a) = a
renderLines (ManyLines a nl ls) = a <> renderNewline nl <> renderLines ls

instance Semigroup a => Semigroup (Lines a) where
  NoLines <> a = a
  OneLine a <> NoLines = OneLine a
  OneLine a <> OneLine b = OneLine (a <> b)
  OneLine a <> ManyLines b nl ls = ManyLines (a <> b) nl ls
  ManyLines a nl ls <> b = ManyLines a nl (ls <> b)

instance Semigroup a => Monoid (Lines a) where
  mempty = NoLines
  mappend = (<>)

renderWhitespace :: Whitespace -> String
renderWhitespace Space = " "
renderWhitespace Tab = "\t"
renderWhitespace (Continued ws) = "\\\n" <> foldMap renderWhitespace ws

renderNewline :: Newline -> String
renderNewline CR = "\r"
renderNewline LF = "\n"
renderNewline CRLF = "\r\n"

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
renderExpr (String _ b) = show b
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

renderStatement :: Statement v a -> Lines String
renderStatement (Fundef _ ws1 name ws2 params ws3 ws4 nl body) =
  ManyLines firstLine nl restLines
  where
    firstLine =
      "def" <> foldMap renderWhitespace ws1 <> name <>
      foldMap renderWhitespace ws2 <> renderParams params <>
      foldMap renderWhitespace ws3 <> ":" <> foldMap renderWhitespace ws4
    restLines =
      foldMap
        (\(_, a, b, nl) -> maybe id endWith nl $ (foldMap renderWhitespace a <>) <$> renderStatement b)
        (view _Wrapped body)
renderStatement (Return _ ws expr) =
  OneLine $ "return" <> foldMap renderWhitespace ws <> renderExpr expr
renderStatement (Expr _ expr) = OneLine $ renderExpr expr
renderStatement (If _ ws1 expr ws2 ws3 nl body body') =
  ManyLines firstLine nl restLines <> fold elseLines
  where
    firstLine =
      "if" <> foldMap renderWhitespace ws1 <>
      renderExpr expr <> foldMap renderWhitespace ws2 <> ":" <>
      foldMap renderWhitespace ws3
    restLines =
      foldMap
        (\(_, a, b, nl) -> maybe id endWith nl $ (foldMap renderWhitespace a <>) <$> renderStatement b)
        (view _Wrapped body)
    elseLines =
      ManyLines <$>
      fmap
        (\(ws4, ws5, _, _) ->
           "else" <> foldMap renderWhitespace ws4 <> ":" <>
           foldMap renderWhitespace ws4)
        body' <*>
      fmap (\(_, _, nl2, _) -> nl2) body' <*>
      fmap
        (\(_, _, _, body'') ->
           foldMap
             (\(_, a, b, nl) -> maybe id endWith nl $ (foldMap renderWhitespace a <>) <$> renderStatement b)
             (view _Wrapped body''))
        body'
renderStatement (While _ ws1 expr ws2 ws3 nl body) =
  ManyLines
    ("while" <> foldMap renderWhitespace ws1 <> renderExpr expr <>
     foldMap renderWhitespace ws2 <> ":" <> foldMap renderWhitespace ws3)
    nl
    (foldMap
       (\(_, a, b, nl) -> maybe id endWith nl $ (foldMap renderWhitespace a <>) <$> renderStatement b)
       (view _Wrapped body))
renderStatement (Assign _ lvalue ws1 ws2 rvalue) =
  OneLine $
  renderExpr lvalue <> foldMap renderWhitespace ws1 <> "=" <>
  foldMap renderWhitespace ws2 <> renderExpr rvalue
renderStatement (Pass _) = OneLine "pass"
renderStatement (Break _) = OneLine "break"

renderArgs :: CommaSep (Arg v a) -> String
renderArgs a = "(" <> renderCommaSep go a <> ")"
  where
    go (PositionalArg _ expr) = renderExpr expr
    go (KeywordArg _ name expr) = name <> "=" <> renderExpr expr

renderParams :: CommaSep (Param v a) -> String
renderParams a = "(" <> renderCommaSep go a <> ")"
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
renderBinOp (Equals _) = "=="
