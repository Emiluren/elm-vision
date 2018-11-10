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

map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map f list = { head = f list.head, tail = List.map f list.tail }

select : (a -> a -> a) -> NonEmptyList a -> a
select f { head, tail } = List.foldl f head tail

findMin : (a -> comparable) -> NonEmptyList a -> a
findMin f list =
    map (\x -> (x, f x)) list
        |> select
           (\(smaller, smallValue) (x, xValue) ->
                if smallValue < xValue then
                    (smaller, smallValue)
                else
                    (x, xValue))
        |> Tuple.first
