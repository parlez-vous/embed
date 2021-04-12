module Api.Input exposing
    ( apiResponseDecoder
    , commentDecoder
    , commentTreeDecoder
    , interactionsDecoder
    , userInfoDecoder
    , userAndTokenDecoder
    )

{-| Represents the Incoming data from the server.

Includes JSON decoders and types.
-}

import Data exposing (Author(..), CommentVote, Interactions, UserInfo, UserInfoWithToken, ApiToken(..))
import Data.Comment exposing (Comment, CommentTree)
import Json.Decode as D exposing (Decoder)
import Time exposing (Posix)
import Set
import RemoteData
import Data exposing (User(..))


map9 
    : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder value
map9 f da db dc dd de df dg dh di =
    (D.map2 (\f_ i -> f_ i))
        (D.map8 f da db dc dd de df dg dh)
        di


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
    -> Posix
    -> Maybe UserInfo
    -> Comment
intoComment id maybeParentCommentId anonAuthorName body replyIds isLeaf createdAt maybeUserInfo =
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

        author =
            case maybeUserInfo of
                Just userInfo ->
                    Authenticated_ userInfo

                Nothing ->
                    Anonymous_ anonAuthorName

        isFolded = False
    in
    Comment
        id
        isLeaf
        maybeParentCommentId
        author
        isFolded
        anonAuthorName
        body
        replyIdSet
        commentReplyBufferState
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
        (D.field "createdAt" timestampDecoder)
        (D.field "author" <| D.maybe userInfoDecoder)


commentTreeDecoder : Decoder CommentTree
commentTreeDecoder =
    let
        setDecoder =
            D.list D.string
            |> D.map Set.fromList
    in
    D.map3 CommentTree
        (D.field "comments" <| D.dict commentDecoder)
        (D.field "topLevelComments" setDecoder)
        (D.field "postId" D.string)



userInfoDecoder : Decoder UserInfo
userInfoDecoder =
    D.map4
        (\id username createdAt updatedAt ->
            { id = id
            , username = username
            , created = createdAt
            , updated = updatedAt
            , isModerator = False
            }
        )
        (D.field "id" D.string)
        (D.field "username" D.string)
        (D.field "createdAt" timestampDecoder)
        (D.field "updatedAt" timestampDecoder)


userAndTokenDecoder : Decoder UserInfoWithToken
userAndTokenDecoder =
    D.map2 (\userInfo tokenString -> (userInfo, ApiToken tokenString))
        (apiResponseDecoder userInfoDecoder)
        (D.field "sessionToken" D.string)


interactionsDecoder : Decoder Interactions
interactionsDecoder =
    let
        commentVotesDecoder =
            D.map2 CommentVote
                (D.field "value" D.int)
                (D.field "commentId" D.string)
    in
    D.map Interactions
        (D.field "commentVotes" <| D.dict commentVotesDecoder)


