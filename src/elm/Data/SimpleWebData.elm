module Data.SimpleWebData exposing (SimpleWebData(..), mapSimpleWebData)

import Http



-- There is no such thing as "not asked" for this data type


type SimpleWebData a
    = Loading
    | Success a
    | Failure Http.Error


mapSimpleWebData : (a -> b) -> SimpleWebData a -> SimpleWebData b
mapSimpleWebData f simpleWebData =
    case simpleWebData of
        Success data ->
            Success (f data)

        Loading ->
            Loading

        Failure e ->
            Failure e
