module Array.Extra exposing (..)

import Array exposing (Array)

detectIndex : (a -> Bool) -> Array a -> Maybe Int
detectIndex f =
  Array.indexedMap (\i a -> (i, f a))
    >> Array.filter Tuple.second
    >> Array.get 0
    >> Maybe.map Tuple.first

detectIndex2d : (a -> Bool) -> Array (Array a) -> Maybe (Int, Int)
detectIndex2d f =
  Array.toIndexedList
    >> List.filterMap (\(i, a)-> detectIndex f a |> Maybe.map ((,) i))
    >> List.head
