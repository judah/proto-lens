{-# LANGUAGE OverloadedStrings #-}
module Main where

import Proto.Oneof
import Data.ProtoLens
import Lens.Family2 ((&), (.~), (^.))
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder (Builder, byteString)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Data.ProtoLens.TestUtil

defFoo :: Foo
defFoo = def

taggedValue :: String -> Builder
taggedValue = tagged 2 . Lengthy . byteString . C.pack


main :: IO ()
main = testMain
    [ serializeTo "default" defFoo "" mempty
    , serializeTo "to first oneof field"
        (defFoo & bippy .~ "querty" & baz .~ 42)
        "baz: 42"
        (tagged 1 $ VarInt 42)
    , serializeTo "to second oneof field"
        (defFoo & baz .~ 42 & bippy .~ "querty")
        "bippy: \"querty\""
        (tagged 2 $ Lengthy "querty")
    , testCase "distinctOneofs" distinctOneofs
    -- Check that we can tolerate missing keys and values.
    , deserializeFrom "from first oneof field"
        (Just $ defFoo & baz .~ 42)
        $ tagged 1 $ VarInt $ 42
    , deserializeFrom "from second oneof field"
        (Just $ defFoo & bippy .~ "querty")
        $ tagged 2 $ Lengthy "querty"
    ]

-- | Test that fields in different "oneof" groups don't clobber each other.
distinctOneofs :: IO ()
distinctOneofs = do
    Nothing @=? ((defFoo & baz .~ 42) ^. maybe'baz2)
    Nothing @=? ((defFoo & baz2 .~ 42) ^. maybe'baz)
    let both = defFoo & baz .~ 42 & baz2 .~ 17
    42 @=? (both ^. baz)
    17 @=? (both ^. baz2)
    "querty" @=? ((defFoo & bippy .~ "querty" & baz2 .~ 17) ^. bippy)
