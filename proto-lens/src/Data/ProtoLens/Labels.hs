-- | This module provides OverloadedLabels 'IsLabel' support by exporting
-- an orphan instance. In modules with access to that orphan instance,
-- the field lenses of a protocol buffer type may be referenced by the
-- expression @#foo@.
--
-- For example, if we have the file @foo.proto@:
--
-- > syntax = "proto3";
-- > message Foo {
-- >   int32 bar = 1
-- >   repeated int32 baz = 2
-- > }
--
-- Then we can reference its fields as follows:
--
-- > {-# LANGUAGE OverloadedLabels #-}
-- > import Data.ProtoLens.Labels ()
-- > import Data.ProtoLens (defMessage)
-- > import Lens.Family2 ((&), (.~))
-- > import Proto.Foo (Foo)
-- >
-- > foo :: Foo
-- > foo = defMessage
-- >           & #bar .~ 42
-- >           & #baz .~ [1..10]

module Data.ProtoLens.Labels () where

import Lens.Labels.Unwrapped ()
