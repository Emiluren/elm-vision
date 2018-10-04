module NonEmptyList exposing (..)

import List

type alias NonEmptyList a =
    { head : a
    , tail : List a
    }

fromList : List a -> Maybe (NonEmptyList a)
fromList list =
    case list of
        [] -> Nothing
        (x::xs) -> Just { head = x, tail = xs }

foldl : (a -> b -> b) -> NonEmptyList a -> b
foldl f { head, tail } = List.foldl f head tail

findMin : (a -> comparable) -> NonEmptyList a -> a
findMin f = foldl compareMin Nothing
