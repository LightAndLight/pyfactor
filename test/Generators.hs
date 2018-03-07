{-# language DataKinds #-}
module Generators where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.Internal.Syntax

genNewline :: MonadGen m => m Newline
genNewline = Gen.element [CR, LF, CRLF]

genWhitespace :: MonadGen m => m Whitespace
genWhitespace =
  Gen.recursive Gen.choice
    [pure Space, pure Tab]
    [Continued <$> genNewline <*> genWhitespaces]

genWhitespaces :: MonadGen m => m [Whitespace]
genWhitespaces = Gen.list (Range.linear 0 10) genWhitespace

genString :: MonadGen m => m String
genString = Gen.list (Range.linear 0 100) Gen.unicode

genCommaSep :: MonadGen m => Range Int -> m a -> m (CommaSep a)
genCommaSep r m = do
  s <- Gen.integral_ r
  Gen.sized $ \n -> go (n `div` Size s) s
  where
    go s 0 = pure CommaSepNone
    go s 1 =
      Gen.choice
        [ CommaSepOne <$> Gen.resize s m
        , CommaSepMany <$>
          Gen.resize s m <*>
          genWhitespaces <*>
          genWhitespaces <*>
          go s 0
        ]
    go s n =
      CommaSepMany <$>
      Gen.resize s m <*>
      genWhitespaces <*>
      genWhitespaces <*>
      go s (n-1)

genArg :: MonadGen m => m (Arg '[] ())
genArg =
  Gen.choice
    [ PositionalArg () <$> genExpr
    , KeywordArg () <$> genString <*> genWhitespaces <*> genWhitespaces <*> genExpr
    ]

genExpr :: MonadGen m => m (Expr '[] ())
genExpr =
  Gen.choice
    [ genList
    , genDeref
    , genCall
    , genNone
    , genBinOp
    , genNegate
    , genParens
    , Ident () <$> genString
    , genInt
    , genBool
    , String () <$> genString
    ]
  where
    genList =
      List () <$>
      genWhitespaces <*>
      genCommaSep (Range.linear 0 10) (Gen.small genExpr) <*>
      genWhitespaces
    genDeref =
      Deref () <$>
      Gen.small genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genString
    genCall =
      Call () <$>
      Gen.small genExpr <*>
      genWhitespaces <*>
      genCommaSep (Range.linear 0 10) genArg
    genNone = pure $ None ()
    genBool = Bool () <$> Gen.bool
    genBinOp =
      BinOp () <$>
      Gen.small genExpr <*>
      genWhitespaces <*>
      genOp <*>
      genWhitespaces <*>
      Gen.small genExpr
    genOp = Gen.element $ _opOperator <$> operatorTable
    genNegate = Negate () <$> genWhitespaces <*> Gen.small genExpr
    genParens = Parens () <$> genWhitespaces <*> Gen.small genExpr <*> genWhitespaces
    genInt = Int () <$> Gen.integral (Range.constant (-2^16) (2^16))
