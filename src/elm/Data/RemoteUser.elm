module Data.RemoteUser exposing
    ( RemoteUser(..)
    , setInteractions
    , setUserInfo
    )


import Api exposing (ApiRequestOutcome)
import Data exposing (Interactions, User(..), UserInfoWithToken)

{-| Represents a combined async data structure
    we do 2 parallel requests and combine them once they're both complete

    just like Promise.all

    there isn't a failure state because when a http request fails, we just fall back to the anonymous user
-}
type RemoteUser
    = AwaitingUserInfoAndInteractions
    | ReceivedUserInfo UserInfoWithToken
    | ReceivedInteractions Interactions
    -- recall that User is a union type
    -- and authenticated user contains UserInfo and Interactions
    | UserLoaded User 



setUserInfo : Maybe String -> ApiRequestOutcome UserInfoWithToken -> RemoteUser -> RemoteUser
setUserInfo fallbackAnonUsername outcome user =
    let
        fallbackOnError = UserLoaded (Anonymous fallbackAnonUsername)
    in
    case user of
        ReceivedInteractions interactions ->
            outcome
            |> Result.map (\(userInfo, apiToken)-> UserLoaded (Authenticated userInfo interactions apiToken))
            |> Result.withDefault fallbackOnError

        AwaitingUserInfoAndInteractions ->
            outcome
            |> Result.map ReceivedUserInfo
            |> Result.withDefault fallbackOnError

        UserLoaded loadedUser -> UserLoaded loadedUser
        ReceivedUserInfo userInfoWithToken -> ReceivedUserInfo userInfoWithToken


setInteractions : Maybe String -> ApiRequestOutcome Interactions -> RemoteUser -> RemoteUser
setInteractions fallbackAnonUsername outcome user =
    let
        fallbackOnError = UserLoaded (Anonymous fallbackAnonUsername)
    in
    case user of
        ReceivedUserInfo (userInfo, apiToken) -> 
            outcome
            |> Result.map (\interactions -> UserLoaded <| Authenticated userInfo interactions apiToken)
            |> Result.withDefault fallbackOnError

        AwaitingUserInfoAndInteractions ->
            outcome
            |> Result.map ReceivedInteractions
            |> Result.withDefault fallbackOnError

        UserLoaded loadedUser -> UserLoaded loadedUser
        ReceivedInteractions interactions -> ReceivedInteractions interactions

