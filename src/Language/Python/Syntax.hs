{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language OverloadedLists #-}
module Language.Python.Syntax where

import Data.String
import Language.Python.Internal.Syntax

class HasPositional p v | p -> v where
  p_ :: v -> p

class HasKeyword p where
  k_ :: String -> Expr '[] () -> p

instance HasPositional (Param '[] ()) String where; p_ = PositionalParam ()
instance HasKeyword (Param '[] ()) where; k_ = KeywordParam ()

def_ :: String -> [Param '[] ()] -> [Statement '[] ()] -> Statement '[] ()
def_ name params block =
  Fundef ()
    [Space]
    name
    []
    params
    []
    []
    LF
    (Block $ (\a -> (,,,) () [Space, Space, Space, Space] a $ Just LF) <$> block)

data Argument = AP (Expr '[] ()) | AK String (Expr '[] ())
instance HasPositional Argument (Expr '[] ()) where; p_ = AP
instance IsString Argument where fromString = AP . fromString

mkArg :: Argument -> Arg '[] ()
mkArg (AP expr) = PositionalArg () expr
mkArg (AK name expr) = KeywordArg () name expr

call_ :: Expr '[] () -> [Argument] -> Expr '[] ()
call_ expr args = Call () expr [] (fmap mkArg args)

return_ :: Expr '[] () -> Statement '[] ()
return_ = Return () [Space]

expr_ :: Expr '[] () -> Statement '[] ()
expr_ = Expr ()

list_ :: [Expr '[] ()] -> Expr '[] ()
list_ es = List () [] (listToCommaSep es) []

is_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
is_ a = BinOp () a [Space] (Is ()) [Space]
infixl 1 `is_`

(.==) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.==) a = BinOp () a [Space] (Equals ()) [Space]
infixl 1 .==

(.|) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.|) = undefined
infixl 2 .|

(.^) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.^) = undefined
infixl 3 .^

(.&) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.&) = undefined
infixl 4 .&

(.<<) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.<<) = undefined
infixl 5 .<<

(.>>) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.>>) = undefined
infixl 5 .>>

(.+) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.+) = (+)
infixl 6 .+

(.-) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.-) = (-)
infixl 6 .-

(.*) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.*) = (*)
infixl 7 .*

(.@) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.@) = undefined
infixl 7 .@

(./) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(./) a = BinOp () a [Space] (Divide ()) [Space]
infixl 7 ./

(.//) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.//) = undefined
infixl 7 .//

(.%) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.%) = undefined
infixl 7 .%

(.**) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.**) a = BinOp () a [Space] (Exp ()) [Space]
infixr 8 .**

(/>) :: Expr '[] () -> String -> Expr '[] ()
(/>) a = Deref () a [] []
infixl 9 />

neg :: Expr '[] () -> Expr '[] ()
neg = negate

if_ :: Expr '[] () -> [Statement '[] ()] -> Statement '[] ()
if_ e sts =
  If () [Space] e [] [] LF
    (Block $ (\a -> (,,,) () [Space, Space, Space, Space] a $ Just LF) <$> sts)
    Nothing

while_ :: Expr '[] () -> [Statement '[] ()] -> Statement '[] ()
while_ e sts =
  While () [Space] e
    [] [] LF
    (Block $ (\a -> (,,,) () [Space, Space, Space, Space] a $ Just LF) <$> sts)

ifElse_ :: Expr '[] () -> [Statement '[] ()] -> [Statement '[] ()] -> Statement '[] ()
ifElse_ e sts sts' =
  If () [Space] e [] [] LF
    (Block $ (\a -> (,,,) () [Space, Space, Space, Space] a $ Just LF) <$> sts)
    (Just ([], [], LF, Block $ (\a -> (,,,) () [Space, Space, Space, Space] a $ Just LF) <$> sts'))

var_ :: String -> Expr '[] ()
var_ = Ident ()

none_ :: Expr '[] ()
none_ = None ()

pass_ :: Statement '[] ()
pass_ = Pass ()

break_ :: Statement '[] ()
break_ = Break ()

true_ :: Expr '[] ()
true_ = Bool () True

false_ :: Expr '[] ()
false_ = Bool () False

and_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
and_ a = BinOp () a [Space] (BoolAnd ()) [Space]

or_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
or_ a = BinOp () a [Space] (BoolOr ()) [Space]

str_ :: String -> Expr '[] ()
str_ = String ()

(.=) :: Expr '[] () -> Expr '[] () -> Statement '[] ()
(.=) a = Assign () a [Space] [Space]
