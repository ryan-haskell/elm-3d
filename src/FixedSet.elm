module FixedSet exposing (FixedSet, init, insert, toAverage)

import Dict exposing (Dict)


type FixedSet value
    = FixedSet { dict : Dict Int value, insertIndex : Int, maxSize : Int }


init : { maxSize : Int } -> FixedSet value
init options =
    FixedSet
        { maxSize = options.maxSize
        , insertIndex = 0
        , dict = Dict.empty
        }


insert : value -> FixedSet value -> FixedSet value
insert value (FixedSet set) =
    FixedSet
        { set
            | insertIndex =
                (set.insertIndex + 1)
                    |> Basics.modBy set.maxSize
            , dict = Dict.insert set.insertIndex value set.dict
        }


toAverage : FixedSet Float -> Maybe Float
toAverage (FixedSet set) =
    let
        sum : Float
        sum =
            Dict.values set.dict
                |> List.sum

        count : Float
        count =
            Dict.size set.dict
                |> Basics.toFloat
    in
    if count == 0 then
        Nothing

    else
        Just (sum / count)
