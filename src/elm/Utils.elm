port module Utils exposing (humanReadableTimestamp, getPathFromUrl, writeToLocalStorage, simpleUpdate, timeDiff, removeToken)

import Time exposing (Posix, posixToMillis)
import Url exposing (Url)





simpleUpdate : m -> ( m, Cmd msg )
simpleUpdate m = ( m, Cmd.none )



--- Ports


-- ( String = key, String = value )
port writeToLocalStorage : ( String, String ) -> Cmd msg
port removeToken : () -> Cmd msg




--- URL Utils

getPathFromUrl : Url -> String
getPathFromUrl url =
    if url.path == "/" then
        "root"
    else
        url.path




--- Time Stuff 


timeDiff : Posix -> Posix -> Int
timeDiff first second =
    posixToMillis first - posixToMillis second



-- Constants 

one_minute : Int
one_minute = 1000 * 60

one_hour : Int
one_hour = one_minute * 60

one_day : Int
one_day = one_hour * 24

one_week : Int
one_week = one_day * 7

one_month : Int
one_month = one_day * 30

one_year : Int
one_year = one_month * 12



--- Conversions

msToSec : Int -> Int
msToSec val = 
    val // 1000

msToMin : Int -> Int
msToMin val =
    msToSec val // 60

msToHours : Int -> Int
msToHours val =
    msToMin val // 60

msToDays : Int -> Int
msToDays val =
    msToHours val // 24

msToWeeks : Int -> Int
msToWeeks val =
    msToDays val // 7

msToMonths : Int -> Int
msToMonths val =
    msToDays val // 30

msToYears : Int -> Int
msToYears val =
    msToMonths val // 12






maybePluralizeLabel : Int -> String -> String
maybePluralizeLabel value label =
    let
        valStr = String.fromInt value 
    in
    if value == 1 then
        valStr ++ " " ++ label ++ " ago"
    else
        valStr ++ " " ++ label ++ "s ago"
        

humanReadableTimestamp : Posix -> Posix -> String
humanReadableTimestamp reference date =
    let
        differenceMs = timeDiff reference date
    in
    if differenceMs < 0 then
        ""
    else if differenceMs < 1000 then
        "just now"
    else if differenceMs < one_minute then
        maybePluralizeLabel (msToSec differenceMs) "second"

    else if differenceMs < one_hour then
        maybePluralizeLabel (msToMin differenceMs) "minute"

    else if differenceMs < one_day then
        maybePluralizeLabel (msToHours differenceMs) "hour"

    else if differenceMs < one_week then
        maybePluralizeLabel (msToDays differenceMs) "day"

    else if differenceMs < one_month then
        maybePluralizeLabel (msToWeeks differenceMs) "week"

    else if differenceMs < one_year then
        maybePluralizeLabel (msToMonths differenceMs) "month" 

    else
        maybePluralizeLabel (msToYears differenceMs) "year"

