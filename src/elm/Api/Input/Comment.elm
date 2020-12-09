module Api.Input.Comment exposing (Comment, commentTreeListDecoder, Replies(..), getReplies)

{-| Comment Related Decoders and Types
-}

import Json.Decode as D exposing (Decoder)
import RemoteData exposing (WebData)
import Time exposing (Posix)


type alias Comment =
    { id : String
    , anonymousAuthorName : String
    , body : String
    , replies : WebData Replies
    , votes : Int
    , createdAt : Posix
    }

type Replies = Replies (List Comment)

getReplies : Replies -> List Comment
getReplies (Replies replies) = replies


timestampDecoder : Decoder Posix
timestampDecoder =
    D.map Time.millisToPosix D.int


intoComment
    : List String
    -> String
    -> String
    -> String
    -> Maybe Replies
    -> Int
    -> Posix
    -> Comment
intoComment leafIds commentId anonAuthorName body maybeReplies votes createdAt =
    let
        -- convert Maybe Replies
        -- into WebData Replies
        repliesRemoteData =
            case maybeReplies of
                Just replies ->
                    RemoteData.Success replies

                -- if the server responded with `null` then
                -- it could mean that there are more replies
                Nothing ->
                    let
                        commentIsALeaf =
                            leafIds
                            |> List.filter (\id -> id == commentId)
                            |> (not << List.isEmpty)
                    in
                    if commentIsALeaf then
                        RemoteData.Success <| Replies []
                    else
                        RemoteData.NotAsked
    in
    Comment commentId anonAuthorName body repliesRemoteData votes createdAt
    



commentTreeDecoder : List String -> Decoder Comment
commentTreeDecoder leafIds =
    D.map6 (intoComment leafIds)
        (D.field "id" D.string)
        (D.field "anon_author_name" D.string)
        (D.field "body" D.string)
        (D.field "replies" <| repliesDecoder leafIds)
        (D.field "votes" D.int)
        (D.field "created_at" timestampDecoder)



repliesDecoder : List String -> Decoder (Maybe Replies)
repliesDecoder leafIds =
    D.lazy (\_ -> D.list <| commentTreeDecoder leafIds)
    |> D.map Replies
    |> D.maybe


commentTreeListDecoder : List String -> Decoder (List Comment)
commentTreeListDecoder leafIds =
    D.list (commentTreeDecoder leafIds)


