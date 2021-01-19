module Data.RemoteUser exposing
    ( RemoteUser(..)
    , setInteractions
    , setUserInfo
    )


import Api exposing (ApiRequestOutcome)
import Data exposing (Interactions, User(..), UserInfo)

{-| Represents a combined async data structure
    we do 2 parallel requests and combine them once they're both complete

    just like Promise.all

    there isn't a failure state because when a http request fails, we just fall back to the anonymous user
-}
type RemoteUser
    = AwaitingUserInfoAndInteractions
    | ReceivedUserInfo UserInfo 
    | ReceivedInteractions Interactions
    -- recall that User is a union type
    -- and authenticated user contains UserInfo and Interactions
    | UserLoaded User 



setUserInfo : Maybe String -> ApiRequestOutcome UserInfo -> RemoteUser -> RemoteUser
setUserInfo fallbackAnonUsername outcome user =
    let
        fallbackOnError = UserLoaded (Anonymous fallbackAnonUsername)
    in
    case user of
        ReceivedInteractions interactions ->
            outcome
            |> Result.map (\info -> UserLoaded (Authenticated info interactions))
            |> Result.withDefault fallbackOnError

        AwaitingUserInfoAndInteractions ->
            outcome
            |> Result.map ReceivedUserInfo
            |> Result.withDefault fallbackOnError

        UserLoaded loadedUser -> UserLoaded loadedUser
        ReceivedUserInfo userInfo -> ReceivedUserInfo userInfo


setInteractions : Maybe String -> ApiRequestOutcome Interactions -> RemoteUser -> RemoteUser
setInteractions fallbackAnonUsername outcome user =
    let
        fallbackOnError = UserLoaded (Anonymous fallbackAnonUsername)

        _ = Debug.log "setInteractions called: " outcome
    in
    case user of
        ReceivedUserInfo userInfo -> 
            outcome
            |> Result.map (UserLoaded << Authenticated userInfo)
            |> Result.withDefault fallbackOnError

        AwaitingUserInfoAndInteractions ->
            outcome
            |> Result.map ReceivedInteractions
            |> Result.withDefault fallbackOnError

        UserLoaded loadedUser -> UserLoaded loadedUser
        ReceivedInteractions interactions -> ReceivedInteractions interactions

