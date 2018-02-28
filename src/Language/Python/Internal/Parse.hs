module Language.Python.Internal.Parse where

import Text.Parser
import Text.Trifecta.Combinators

annotated :: DeltaParsing m => m (Span -> b) -> m b
annotated m = (\(f :~ sp) -> f sp) <$> spanned m

bool :: CharParsing m => m (a -> Expr '[] a)
bool =
  fmap (\b a -> Bool a b) $
  (string "True" $> True) <|>
  (string "False" $> False)

none :: CharParsing m => m (a -> Expr '[] a)
none =
  string "None" $> None

strLit :: CharParsing m => m (a -> Expr '[] a)
strLit =
  fmap (\b a -> String a b) $
  char '"' *>
  manyTill letter (char '"')
