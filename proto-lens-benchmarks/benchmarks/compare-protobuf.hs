-- Replicating some benchmarks from the 'protobuf` package for comparison.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC.Generics (Generic(..))
import Data.ProtoLens
import Lens.Family2 ((&), (.~))
import Proto.CompareProtobuf
    ( Burst(..)
    , A(..)
    , B'Entry(..)
    , B(..)
    , C'Entry(..)
    , C(..)
    , as
    , bs
    , frames
    , key
    , value
    , w
    , x
    , y
    , z
    )

burst :: Int -> Burst
burst n = def & frames .~ replicate n "abcd"

decBurst :: B.ByteString -> Burst
decBurst = decodeMessageOrDie

encodeBurst :: Int -> B.ByteString
encodeBurst = encodeMessage . burst

deriving instance Generic Burst
deriving instance NFData Burst
deriving instance Generic A
deriving instance NFData A
deriving instance Generic B'Entry
deriving instance NFData B'Entry
deriving instance Generic B
deriving instance NFData B
deriving instance Generic C'Entry
deriving instance NFData C'Entry
deriving instance Generic C
deriving instance NFData C

decNested :: B.ByteString -> C
decNested = decodeMessageOrDie

nested :: Int -> Int -> C
nested i j = def & bs .~ zipWith mkEntryC (map show' [0..i]) (replicate i b)
  where
    show' = Text.pack . show
    b = def & as .~ zipWith mkEntryB (map show' [0..j]) (replicate j a)
    a = def & w .~ 0.1 & x .~ 0.2 & y .~ 0.3 & z .~ 0.4
    mkEntryB :: Text.Text -> A -> B'Entry
    mkEntryB k v = def & key .~ k & value .~ v
    mkEntryC :: Text.Text -> B -> C'Entry
    mkEntryC k v = def & key .~ k & value .~ v

main :: IO ()
main = defaultMain [
    bgroup "burst" [
        bgroup "encoding" [
            bench "1" $ nf encodeMessage (burst 1),
            bench "10" $ nf encodeMessage (burst 10),
            bench "100" $ nf encodeMessage (burst 100),
            bench "1000" $ nf encodeMessage (burst 1000)
        ],
        bgroup "decoding" [
            bench "1" $ nf decBurst (encodeBurst 1),
            bench "10" $ nf decBurst (encodeBurst 10),
            bench "100" $ nf decBurst (encodeBurst 100),
            bench "1000" $ nf decBurst (encodeBurst 1000)
            ]
    ],
    bgroup "nested" [
        bgroup "encoding" [
            bench "a1" $ nf encodeMessage (nested 1 1),
            bench "a10" $ nf encodeMessage (nested 1 10),
            bench "a100" $ nf encodeMessage (nested 1 100),
            bench "a1000" $ nf encodeMessage (nested 1 1000),
            bench "b10" $ nf encodeMessage (nested 10 1),
            bench "b100" $ nf encodeMessage (nested 100 1),
            bench "b1000" $ nf encodeMessage (nested 1000 1)
        ],
        bgroup "decoding" [
            bench "a1" $ nf decNested (encodeMessage $ nested 1 1),
            bench "a10" $ nf decNested (encodeMessage $ nested 1 10),
            bench "a100" $ nf decNested (encodeMessage $ nested 1 100),
            bench "a1000" $ nf decNested (encodeMessage $ nested 1 1000),
            bench "b10" $ nf decNested (encodeMessage $ nested 1 10),
            bench "b100" $ nf decNested (encodeMessage $ nested 1 100),
            bench "b1000" $ nf decNested (encodeMessage $ nested 1 1000)
        ]
    ]
  ]
        
