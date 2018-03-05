{-# language DataKinds #-}
module Language.Python.Internal.Parse where

import Control.Applicative
import Control.Lens hiding (List, argument)
import Data.Foldable
import Data.Functor
import Data.Semigroup hiding (Arg)
import Text.Parser.Token hiding (commaSep)
import Text.Trifecta hiding (newline, commaSep)

import Language.Python.Internal.Syntax

type Untagged s a = a -> s '[] a

newline :: CharParsing m => m Newline
newline =
  char '\n' $> LF <|>
  char '\r' *> (char '\n' $> CRLF <|> pure CR)

annotated :: DeltaParsing m => m (Untagged b Span) -> m (b '[] Span)
annotated m = (\(f :~ sp) -> f sp) <$> spanned m

whitespace :: CharParsing m => m Whitespace
whitespace =
  (char ' ' $> Space) <|>
  (char '\t' $> Tab) <|>
  (Continued <$> newline <*> many whitespace)

identifier :: (TokenParsing m, Monad m) => m String
identifier = runUnspaced $ ident idStyle

commaSep :: (CharParsing m, Monad m) => m a -> m (CommaSep a)
commaSep e = someCommaSep <|> pure CommaSepNone
  where
    someCommaSep = do
      val <- e
      res <-
        optional $
          CommaSepMany val <$>
          many whitespace <* char ',' <*>
          many whitespace <*> commaSep e
      case res of
        Nothing -> pure $ CommaSepOne val
        Just a -> pure a

argument :: DeltaParsing m => m (Untagged Arg Span)
argument = kwarg <|> posarg
  where
    kwarg =
      try
        ((\a b c d e -> KeywordArg e a b c d) <$>
        identifier <*>
        many whitespace <*
        char '=') <*>
      many whitespace <*> expr
    posarg = flip PositionalArg <$> expr

expr :: DeltaParsing m => m (Expr '[] Span)
expr = orExpr
  where
    atom =
      annotated $
      bool <|>
      none <|>
      strLit <|>
      int <|>
      (flip Ident <$> identifier) <|>
      list <|>
      parenthesis

    list =
      (\a b c d -> List d a b c) <$
      char '[' <*> many whitespace <*> commaSep expr <*> many whitespace <* char ']'

    bool = fmap (flip Bool) $
      (string "True" $> True) <|>
      (string "False" $> False)

    none =
      string "None" $> None

    strLit =
      fmap (flip String) $
      char '"' *>
      manyTill letter (char '"')

    int = (\a b -> Int b $ read a) <$> some digit

    parenthesis =
      (\a b c d -> Parens d a b c) <$>
      (char '(' *> many whitespace) <*>
      expr <*>
      (many whitespace <* char ')')

    binOpL inner p = chainl1 inner $ do
      (ws1, op, s) <- try $ do
        ws1 <- many whitespace
        op :~ s <- spanned p
        pure (ws1, op, s)
      ws2 <- many whitespace
      pure $ \a b -> BinOp (a ^. exprAnnotation <> b ^. exprAnnotation) a ws1 (op $> s) ws2 b

    orExpr = binOpL andExpr (reserved "or" $> BoolOr ())

    andExpr = binOpL notExpr (reserved "and" $> BoolAnd ())

    notExpr = comparison

    comparison = binOpL bitOr $
      reserved "is" $> Is () <|>
      string "==" $> Equals ()

    bitOr = bitXor

    bitXor = bitAnd

    bitAnd = bitShift

    bitShift = arith

    arith = binOpL term $
      char '+' $> Plus () <|>
      char '-' $> Minus ()

    term = binOpL factor $
      char '*' $> Multiply () <|>
      char '/' $> Divide ()

    factor =
      annotated ((\a b c -> Negate c a b) <$ char '-' <*> many whitespace <*> expr) <|>
      power

    power = do
      a <- atomExpr
      v <-
        optional
          (try ((,,,) <$> many whitespace <*> spanned (string "**")) <*>
           many whitespace <*>
           factor)
      case v of
        Nothing -> pure a
        Just (ws1, _ :~ s, ws2, b) ->
          pure $ BinOp (a ^. exprAnnotation <> b ^. exprAnnotation) a ws1 (Exp s) ws2 b

    atomExpr =
      (\a afters -> case afters of; [] -> a; _ -> foldl' (\b f -> f b) a afters) <$>
      atom <*>
      many (deref <|> call)
      where
        deref =
          (\ws1 ws2 (str :~ s) a -> Deref (a ^. exprAnnotation <> s) a ws1 ws2 str) <$>
          try (many whitespace <* char '.') <*>
          many whitespace <*>
          spanned identifier
        call =
          (\ws1 (csep :~ s) a -> Call (a ^. exprAnnotation <> s) a ws1 csep) <$>
          try (many whitespace <* char '(') <*>
          spanned (commaSep (annotated argument) <* char ')')
