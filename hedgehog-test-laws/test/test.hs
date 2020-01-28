{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative (liftA2)
import           Control.Monad (ap)

import           Data.Functor.Classes (Eq1(..))

import           Hedgehog.Internal.Tree (TreeT(..), NodeT(..))

import qualified Test.QuickCheck as QC
import           Test.QuickCheck (arbitrary1, choose, vector, Arbitrary(..), Arbitrary1(..), CoArbitrary(..))

import           Test.QuickCheck.Checkers (EqProp(..), eq, TestBatch)
import           Test.QuickCheck.Classes ()

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperties)

main :: IO ()
main =
  defaultMain instances

instances ::  TestTree
instances =
  let
    testBatch =
      uncurry testProperties
  in
    testGroup "Instances" [
      testGroup "TreeT" $
        testBatch <$> [
            qc_applicative (undefined :: TreeT Maybe (Bool, Char, Int))
          , qc_monad (undefined :: TreeT Maybe (Bool, Char, Int))
          ]
    , testGroup "NodeT" $
        testBatch <$> [
            qc_applicative (undefined :: NodeT Maybe (Bool, Char, Int))
          , qc_monad (undefined :: NodeT Maybe (Bool, Char, Int))
          ]
    ]

------------------------------------------------------------------------
-- Machinery

-- | Properties to check that the 'Applicative' @m@ satisfies the applicative
-- properties
qc_applicative :: forall m a b c.
               ( Applicative m
               , Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary (m a)
               , Arbitrary (m (b -> c)), Show (m (b -> c))
               , Arbitrary (m (a -> b)), Show (m (a -> b))
               , Show a, Show (m a)
               , EqProp (m a), EqProp (m b), EqProp (m c)
               ) =>
               m (a,b,c) -> TestBatch
qc_applicative = const ( "applicative"
                    , [ ("identity"    , QC.property identityP)
                      , ("composition" , QC.property compositionP)
                      , ("homomorphism", QC.property homomorphismP)
                      , ("interchange" , QC.property interchangeP)
                      , ("functor"     , QC.property functorP)
                      ]
                    )
 where
   identityP     :: m a -> QC.Property
   compositionP  :: m (b -> c) -> m (a -> b) -> m a -> QC.Property
   homomorphismP :: (a -> b) -> a -> QC.Property
   interchangeP  :: m (a -> b) -> a -> QC.Property
   functorP      :: (a -> b) -> m a -> QC.Property

   identityP v        = (pure id <*> v) =-= v
   compositionP u v w = (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w))
   homomorphismP f x  = (pure f <*> pure x) =-= (pure (f x) :: m b)
   interchangeP u y   = (u <*> pure y) =-= (pure ($ y) <*> u)
   functorP f x       = (fmap f x) =-= (pure f <*> x)

-- | Properties to check that the 'Monad' @m@ satisfies the monad properties
qc_monad :: forall m a b c.
         ( Monad m
         , Show a, Arbitrary a, CoArbitrary a, CoArbitrary b
         , Arbitrary (m a), EqProp (m a), Show (m a)
         , Arbitrary (m b), EqProp (m b)
         , Arbitrary (m c), EqProp (m c)
         , Show (m (a -> b)), Arbitrary (m (a -> b))
         ) =>
         m (a,b,c) -> TestBatch
qc_monad = const ( "monad laws"
              , [ ("left  identity", QC.property leftP)
                , ("right identity", QC.property rightP)
                , ("associativity" , QC.property assocP)
                , ("pure", QC.property pureP)
                , ("ap", QC.property apP)
                ]
              )
 where
   leftP  :: (a -> m b) -> a -> QC.Property
   rightP :: m a -> QC.Property
   assocP :: m a -> (a -> m b) -> (b -> m c) -> QC.Property
   pureP :: a -> QC.Property
   apP :: m (a -> b) -> m a -> QC.Property

   leftP f a    = (return a >>= f)  =-= f a
   rightP m     = (m >>= return)    =-=  m
   assocP m f g = ((m >>= f) >>= g) =-= (m >>= (\x -> f x >>= g))
   pureP x = (pure x :: m a) =-= return x
   apP f x = (f <*> x) =-= (f `ap` x)

------------------------------------------------------------------------

------------------------------------------------------------------------
-- Orphan instances

-- Tree

instance (Eq1 m, Eq a) => EqProp (TreeT m a) where
  (=-=) =
    eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (TreeT m a) where
  arbitrary =
    TreeT <$> arbitrary1

-- Node

instance (Eq1 m, Eq a) => EqProp (NodeT m a) where
  (=-=) = eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (NodeT m a) where
  arbitrary = do
    n <- choose (0, 2)
    liftA2 NodeT arbitrary (vector n)
