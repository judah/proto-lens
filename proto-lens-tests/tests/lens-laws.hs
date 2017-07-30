{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Lens.Family2 (Lens', view, set, over)
import Data.Int (Int32)
import Data.Functor.Identity (Identity)
import Data.ProtoLens (Message)
import Data.ProtoLens.Arbitrary (arbitraryMessage)
import Proto.LensLaws
import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

main = defaultMain
    [ testGroup "optional" $ testLens (value :: Lens' Optional Int32)
    , testGroup "optional maybe" $ testLens (maybe'value :: Lens' Optional (Maybe Int32))
    , testGroup "oneof" $ testLens (alt1 :: Lens' Oneof Int32)
    , testGroup "oneof maybe" $ testLens (maybe'alt1 :: Lens' Oneof (Maybe Int32))
    ]

testLens :: forall a b . (Message a, Arbitrary b, Show a, Show b, Eq a, Eq b)
         => Lens' a b -> [Test]
testLens l =
    [ testProperty "view l (set l y x) == y" $ forAll arbitraryMessage $ \x y
            -> view l (set l y x) === y
    , testProperty "set l (view l x) x == x" $ forAll arbitraryMessage $ \x
            -> set l (view l x) x === x
    , testProperty "set l y' . set l y == set l y'" $ forAll arbitraryMessage $ \x y y'
            -> set l y' (set l y x) === set l y' x
    -- Simpler versions of law #2:
    , testProperty "over l id == id" $ forAll arbitraryMessage $ \x
            -> over l id x === x
    , testProperty "l pure == pure" $ forAll arbitraryMessage $ \x
            -> l pure x === (pure x :: Identity a)
    ]


