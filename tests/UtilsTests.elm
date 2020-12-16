module UtilsTests exposing (suite)

{-| Tests for the Utils module
-}

import Expect
import Test exposing (..)
import Time exposing (Posix)

import Utils
import Url


msBase : Int
msBase = 1607457410346

nowDate : Posix
nowDate = Time.millisToPosix msBase


type Unit
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks


fromNow : Unit -> Int -> Posix
fromNow unit value =
    Time.millisToPosix <|
        case unit of
            Seconds ->
                msBase - (value * 1000)

            Minutes ->
                msBase - (value * 1000 * 60)

            Hours ->
                msBase - (value * 1000 * 60 * 60)

            Days ->
                msBase - (value * 1000 * 60 * 60 * 24)

            Weeks ->
                msBase - (value * 1000 * 60 * 60 * 24 * 7)


suite : Test
suite =
    describe "Utils module"
        [ describe "getPathFromUrl"
            [ test "Returns 'root' for url with no path" <|
                \_ ->
                    let
                        url = Url.fromString "http://dev.parlezvous.io:8080"

                        maybeSlug = Maybe.map Utils.getPathFromUrl url
                    in
                    Expect.equal (Just "root") maybeSlug

            , test "Returns valid path without initial forward slash" <|
                \_ ->
                    let
                        url = Url.fromString "http://dev.parlezvous.io:8080/abc/one-two-three/gio"

                        maybeSlug = Maybe.map Utils.getPathFromUrl url
                    in
                    Expect.equal (Just "/abc/one-two-three/gio") maybeSlug
            ]

        , describe "humanReadableTimestamp"
            [ test "Converts timestamp into seconds from now" <|
                \_ ->
                    let
                        createdDate = fromNow Seconds 10
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "10 seconds ago" timestamp

            , test "Converts timestamp into minutes from now" <|
                \_ ->
                    let
                        createdDate = fromNow Minutes 15
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "15 minutes ago" timestamp

            , test "Converts timestamp into hours from now" <|
                \_ ->
                    let
                        createdDate = fromNow Hours 20
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "20 hours ago" timestamp

            , test "Converts timestamp into days from now" <|
                \_ ->
                    let
                        createdDate = fromNow Hours 26
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "1 day ago" timestamp

            , test "Converts timestamp into weeks from now" <|
                \_ ->
                    let
                        createdDate = fromNow Days 26
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "3 weeks ago" timestamp

            , test "Converts timestamp into months from now" <|
                \_ ->
                    let
                        createdDate = fromNow Weeks 10
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "2 months ago" timestamp

            , test "Converts timestamp into years from now" <|
                \_ ->
                    let
                        createdDate = fromNow Weeks 54
                        timestamp = Utils.humanReadableTimestamp nowDate createdDate
                    in
                    Expect.equal "1 year ago" timestamp
            ]
        ]

