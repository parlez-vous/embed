module Api.Input exposing
    ( apiResponseDecoder
    , commentDecoder
    , commentTreeDecoder
    )

{-| Represents the Incoming data from the server.

Includes JSON decoders and types.
-}

import Data.Comment exposing (Comment, CommentTree)
import Json.Decode as D exposing (Decoder)
import Time exposing (Posix)
import Set exposing (Set)
import RemoteData


apiResponseDecoder : Decoder a -> Decoder a
apiResponseDecoder dataDecoder =
    D.field "data" dataDecoder


timestampDecoder : Decoder Posix
timestampDecoder =
    D.map Time.millisToPosix D.int



intoComment
    : String
    -> Maybe String
    -> String
    -> String
    -> List String
    -> Bool
    -> Int
    -> Posix
    -> Comment
intoComment id maybeParentCommentId anonAuthorName body replyIds isLeaf votes createdAt =
    let
        replyIdSet = Set.fromList replyIds

        commentReplyBufferState =
            -- if the server didn't respond back with the Comment's replies...
            if Set.isEmpty replyIdSet && not isLeaf then
                RemoteData.NotAsked
            else
                RemoteData.Success ()

        textAreaVisibility = False

        textAreaState = ( textAreaVisibility, "" )
    in
    Comment
        id
        isLeaf
        maybeParentCommentId
        anonAuthorName
        body
        replyIdSet
        commentReplyBufferState
        votes
        createdAt
        textAreaState


commentDecoder : Decoder Comment
commentDecoder = 
    D.map8 intoComment
        (D.field "id" D.string)
        (D.field "parentCommentId" <| D.maybe D.string)
        (D.field "anonAuthorName" D.string)
        (D.field "body" D.string)
        (D.field "replyIds" <| D.list D.string)
        (D.field "isLeaf" D.bool)
        (D.field "votes" D.int)
        (D.field "createdAt" timestampDecoder)


commentTreeDecoder : Decoder CommentTree
commentTreeDecoder =
    let
        setDecoder =
            D.list D.string
            |> D.map Set.fromList
    in
    D.map4 CommentTree
        (D.field "comments" <| D.dict commentDecoder)
        (D.field "topLevelComments" setDecoder)
        (D.field "siteVerified" D.bool)
        (D.field "postId" D.string)

