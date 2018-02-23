{-# language DataKinds #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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
    name
    params
    (Block $ (,,) () [Space, Space, Space, Space] <$> block)

data Arg = AP (Expr '[] ()) | AK String (Expr '[] ())
instance HasPositional Arg (Expr '[] ()) where; p_ = AP
instance IsString Arg where fromString = AP . fromString

mkArgs :: [Arg] -> Args '[] ()
mkArgs [] = NoArgs ()
mkArgs (a:as) =
  case a of
    AP expr -> PositionalArg () expr $ mkArgs as
    AK name expr -> undefined

call_ :: Expr '[] () -> [Arg] -> Expr '[] ()
call_ expr args = Call () expr (mkArgs args)

return_ :: Expr '[] () -> Statement '[] ()
return_ = Return ()

expr_ :: Expr '[] () -> Statement '[] ()
expr_ = Expr ()

list_ :: [Expr '[] ()] -> Expr '[] ()
list_ = List ()

is_ :: Expr '[] () -> Expr '[] () -> Expr '[] ()
is_ = BinOp () (Is ())
infixl 1 `is_`

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
(.+) = undefined
infixl 6 .+

(.-) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.-) = BinOp () (Minus ())
infixl 6 .-

(.*) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.*) = undefined
infixl 7 .*

(.@) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.@) = undefined
infixl 7 .@

(./) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(./) = undefined
infixl 7 ./

(.//) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.//) = undefined
infixl 7 .//

(.%) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.%) = undefined
infixl 7 .%

(.**) :: Expr '[] () -> Expr '[] () -> Expr '[] ()
(.**) = undefined
infixl 8 .**

(/>) :: Expr '[] () -> String -> Expr '[] ()
(/>) = Deref ()
infixl 9 />

if_ :: Expr '[] () -> [Statement '[] ()] -> Statement '[] ()
if_ e sts = If () e (Block $ (,,) () [Space, Space, Space, Space] <$> sts)

var_ :: String -> Expr '[] ()
var_ = Ident ()

none_ :: Expr '[] ()
none_ = None ()

pass_ :: Statement '[] ()
pass_ = Pass ()

(.=) :: Expr '[] () -> Expr '[] () -> Statement '[] ()
(.=) = Assign ()
