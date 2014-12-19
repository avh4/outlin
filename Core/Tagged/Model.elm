module Core.Tagged.Model
  ( Value, Zipper
  , value, toValue
  , toZipper
  ) where

type alias Value t v = (t, v)
type alias Zipper t z = (t, z)

value : t -> v -> Value t v
value t v = (t, v)

toValue : (z -> v) -> Zipper t z -> Value t v
toValue fn (t,z) = (t, fn z)

toZipper : (v -> z) -> Value t v -> Zipper t z
toZipper fn (t,v) = (t, fn v)
