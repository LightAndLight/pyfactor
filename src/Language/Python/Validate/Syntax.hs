{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# language TypeSynonymInstances, FlexibleInstances #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Control.Lens ((#), (^.), _head, _last)
import Control.Lens.Fold
import Control.Lens.Tuple
import Control.Lens.Traversal
import Data.Char
import Data.Coerce
import Data.Semigroup
import Data.Type.Set
import Data.Validate
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax.Error

data Syntax

class StartsWith s where
  startsWith :: s -> Maybe Char

class EndsWith s where
  endsWith :: s -> Maybe Char

isIdentifierChar :: Char -> Bool
isIdentifierChar = liftA2 (||) isLetter (=='_')

instance StartsWith (BinOp a) where
  startsWith Is{} = Just 'i'
  startsWith Minus{} = Just '-'
  startsWith Exp{} = Just '*'
  startsWith BoolAnd{} = Just 'a'
  startsWith BoolOr{} = Just 'o'
  startsWith Multiply{} = Just '*'
  startsWith Divide{} = Just '/'
  startsWith Plus{} = Just '+'
  startsWith Equals{} = Just '='

instance EndsWith (BinOp a) where
  endsWith Is{} = Just 's'
  endsWith Minus{} = Just '-'
  endsWith Exp{} = Just '*'
  endsWith BoolAnd{} = Just 'd'
  endsWith BoolOr{} = Just 'r'
  endsWith Multiply{} = Just '*'
  endsWith Divide{} = Just '/'
  endsWith Plus{} = Just '+'
  endsWith Equals{} = Just '='

instance StartsWith (Expr v a) where
  startsWith List{} = Just '['
  startsWith (Deref _ e _ _ _) = startsWith e
  startsWith (Call _ e _ _) = startsWith e
  startsWith None{} = Just 'N'
  startsWith (BinOp _ e _ _ _ _) = startsWith e
  startsWith Negate{} = Just '-'
  startsWith Parens{} = Just '('
  startsWith (Ident _ s) = s ^? _head
  startsWith (Int _ i) = show i ^? _head
  startsWith (Bool _ b) = show b ^? _head
  startsWith String{} = Just '"'

instance EndsWith (Expr v a) where
  endsWith List{} = Just ']'
  endsWith (Deref _ _ _ _ s) =
    case s of
      [] -> Just '.'
      _ -> s ^? _last
  endsWith Call{} = Just ')'
  endsWith None{} = Just 'e'
  endsWith (BinOp _ _ _ _ _ e) = endsWith e
  endsWith (Negate _ _ e) = endsWith e
  endsWith Parens{} = Just ')'
  endsWith (Ident _ s) = s ^? _last
  endsWith (Int _ i) = show i ^? _last
  endsWith (Bool _ b) = show b ^? _last
  endsWith String{} = Just '"'

instance StartsWith String where
  startsWith a = a ^? _head

instance EndsWith String where
  endsWith a = a ^? _last

validateWhitespace
  :: ( EndsWith x, StartsWith y
     , AsSyntaxError e v a
     )
  => a
  -> (x, x -> String)
  -> [Whitespace]
  -> (y, y -> String)
  -> Validate [e] [Whitespace]
validateWhitespace ann (a, aStr) [] (b, bStr)
  | Just c1 <- endsWith a
  , Just c2 <- startsWith b
  , isIdentifierChar c1
  , isIdentifierChar c2
  = Failure [_MissingSpacesIn # (ann, aStr a, bStr b)]
validateWhitespace _ _ ws _ = Success ws

validateExprSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Expr v a
  -> Validate [e] (Expr (Nub (Syntax ': v)) a)
validateExprSyntax (Parens a ws1 e ws2) = Parens a ws1 <$> validateExprSyntax e <*> pure ws2
validateExprSyntax (Bool a b) = pure $ Bool a b
validateExprSyntax (Negate a ws expr) = Negate a ws <$> validateExprSyntax expr
validateExprSyntax (String a b) = pure $ String a b
validateExprSyntax (Int a n) = pure $ Int a n
validateExprSyntax (Ident a name) = pure $ Ident a name
validateExprSyntax (List a ws1 exprs ws2) =
  List a ws1 <$> traverse validateExprSyntax exprs <*> pure ws2
validateExprSyntax (Deref a expr ws1 ws2 name) =
  Deref a <$>
  validateExprSyntax expr <*>
  pure ws1 <*>
  pure ws2 <*>
  pure name
validateExprSyntax (Call a expr ws args) =
  Call a <$>
  validateExprSyntax expr <*>
  pure ws <*>
  validateArgsSyntax args
validateExprSyntax (None a) = pure $ None a
validateExprSyntax e@(BinOp a e1 ws1 op ws2 e2) =
  BinOp a <$>
  validateExprSyntax e1 <*>
  validateWhitespace a (e1, renderExpr) ws1 (op, renderBinOp) <*>
  pure op <*>
  validateWhitespace a (op, renderBinOp) ws2 (e2, renderExpr) <*>
  validateExprSyntax e2

validateBlockSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Block v a
  -> Validate [e] (Block (Nub (Syntax ': v)) a)
validateBlockSyntax (Block []) = pure $ Block []
validateBlockSyntax (Block [b]) = Block . pure <$> traverseOf _3 validateStatementSyntax b
validateBlockSyntax (Block (b:bs)) =
  fmap Block $
  (:) <$>
  (case b ^. _4 of
     Nothing -> Failure [_ExpectedNewlineAfter # b]
     Just{} -> traverseOf _3 validateStatementSyntax b) <*>
  (fmap unBlock . validateBlockSyntax $ Block bs)

validateStatementSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Statement v a
  -> Validate [e] (Statement (Nub (Syntax ': v)) a)
validateStatementSyntax (Fundef a ws1 name ws2 params ws3 ws4 nl body) =
  Fundef a ws1 name ws2 <$>
  validateParamsSyntax params <*>
  pure ws3 <*>
  pure ws4 <*>
  pure nl <*>
  validateBlockSyntax body
validateStatementSyntax (Return a ws expr) =
  Return a <$>
  validateWhitespace a ("return", id) ws (expr, renderExpr) <*>
  validateExprSyntax expr
validateStatementSyntax (Expr a expr) =
  Expr a <$>
  validateExprSyntax expr
validateStatementSyntax (If a ws1 expr ws2 ws3 nl body body') =
  If a <$>
  validateWhitespace a ("if", id) ws1 (expr, renderExpr) <*>
  validateExprSyntax expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockSyntax body <*>
  traverseOf (traverse._4) validateBlockSyntax body'
validateStatementSyntax (While a ws1 expr ws2 ws3 nl body) =
  While a <$>
  validateWhitespace a ("while", id) ws1 (expr, renderExpr) <*>
  validateExprSyntax expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockSyntax body
validateStatementSyntax (Assign a lvalue rvalue) =
  Assign a <$>
  (if canAssignTo lvalue
   then validateExprSyntax lvalue
   else Failure [_CannotAssignTo # (a, lvalue)]) <*>
  validateExprSyntax rvalue
validateStatementSyntax p@Pass{} = pure $ coerce p
validateStatementSyntax p@Break{} = pure $ coerce p

canAssignTo :: Expr v a -> Bool
canAssignTo None{} = False
canAssignTo _ = True

validateArgsSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Args v a -> Validate [e] (Args (Nub (Syntax ': v)) a)
validateArgsSyntax = go [] False
  where
    go :: (AsSyntaxError e v a, Member Indentation v)
       => [String] -> Bool -> Args v a -> Validate [e] (Args (Nub (Syntax ': v)) a)
    go _ _ [] = pure []
    go names False (PositionalArg a expr : args) =
      liftA2 (:)
        (PositionalArg a <$> validateExprSyntax expr)
        (go names False args)
    go names True (PositionalArg a expr : args) =
      let
        errs = [_PositionalAfterKeywordArg # (a, expr)]
      in
        Failure errs <*> go names True args
    go names _ (KeywordArg a name expr : args)
      | name `elem` names =
          Failure [_DuplicateArgument # (a, name)] <*> go names True args
      | otherwise =
          liftA2 (:)
            (KeywordArg a name <$> validateExprSyntax expr)
            (go (name:names) True args)

validateParamsSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Params v a -> Validate [e] (Params (Nub (Syntax ': v)) a)
validateParamsSyntax = go [] False
  where
    go _ _ [] = pure []
    go names False (PositionalParam a name : params)
      | name `elem` names = 
          Failure [_DuplicateArgument # (a, name)] <*> go (name:names) False params
      | otherwise =
          (PositionalParam a name :) <$> go (name:names) False params
    go names True (PositionalParam a name : params) =
      let errs =
            [_DuplicateArgument # (a, name) | name `elem` names] <>
            [_PositionalAfterKeywordParam # (a, name)]
      in
        Failure errs <*> go (name:names) True params
    go names _ (KeywordParam a name expr : params)
      | name `elem` names =
          Failure [_DuplicateArgument # (a, name)] <*> go names True params
      | otherwise =
          liftA2 (:)
            (KeywordParam a name <$> validateExprSyntax expr)
            (go (name:names) True params)
