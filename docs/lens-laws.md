# Lens laws

There are [three lens laws](https://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens) that specify the behavior we'd expect of a reasonable accessor.  If `l :: Lens s t a b`, then

1. `view l (set l v s) === v`
2. `set l (view l s) s === s`
3. `set l v' (set l v s) === set l v' s`

The lenses that `proto-lens` generates violate law #2, due to how we deal with
optional fields and default values.

We have QuickCheck tests which verify this behavior:

    stack build proto-lens-tests:lens-laws
    stack exec lens-laws

## Example
Suppose we define (in proto2 syntax, though similar issues exist
for proto3):

    message Optional {
      optional int32 value = 1;
    }

Then (ignoring overloading of field names) we generate the following API:

    data Optional = Optional (Maybe Int32)

    maybe'value :: Lens' Optional (Maybe Int32)
    maybe'value = lens (\(Optional x) -> x) (\_ x -> Optional x)

    value :: Lens' Optional Int32
    value = maybe'value . Data.ProtoLens.maybeLens 0 -- the default value

where the library function `maybeLens` is defined by

    maybeLens :: b -> Lens' (Maybe b) b
    maybeLens x = lens (fromMaybe x) $ const Just

In other words, `value` substitutes a default value of zero when none is
present in the proto.

The problem is that although `maybe'value` is indeed a lawful
lens, `maybeLens` (and thus `value` as well) are not.  In particular, it
violates law #2:

    set value (view value (Optional Nothing)) == Optional (Just 0)

or, even simpler:

    over value id (Optional Nothing) == Optional (Just 0)

That is, anytime you change the non-maybe'd value, you "forget" whether the
value had been unset before.

## Analysis
In practice, most uses of protocol buffers don't care about the difference
between a field being unset and it being set to the default value.  In fact,
that type of codified "loose" reasoning allows protocol buffers to support
forwards and backwards compatibility.

Furthermore, the behavior of `set` and `view` in `proto-lens` mirror the
expected behavior of other language bindings.  For example, the generated API
in C++ looks like:

    class Optional {
      public:
        bool has_value();
        int32 value();  // returns 0 when has_value is not set
        void set_value(int32 v);  // Causes the field to be set,
                                  // regardless of the value of `v`.
    };

In our experience, the current behavior strikes a resonable balance between
the different tradeoffs.  The main thing to remember is that by using the
non-`maybe'*` variant of the lens, you're asserting that you don't care
whether the field is set to a default or unset.  If you do want that
distinction, then you should use the `maybe'*` variant instead.

## Could prisms help?
Suppose instead we defined `value` by

    value :: Traversal' Optional Int32
    value = maybe'value . _Just

using the prism `_Just` (here used as a traversal).  Does this help?
Unfortunately, it leads to even more unintuitive behavior:

    set value 42 (def :: Optional) == Optional Nothing

or, focusing on the `_Just` traversal in particular:

    set _Just 42 Nothing == Nothing

since the `_Just` prism traverses over zero elements when it's given a `Nothing` value.


