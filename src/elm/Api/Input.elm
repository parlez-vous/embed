module Api.Input exposing
    ( apiResponseDecoder
    , commentDecoder
    , commentTreeDecoder
    )

{-| Represents the Incoming data from the server.

Includes JSON decoders and types.
-}

import Data.Comment exposing (Comment, CommentTree, intoComment)
import Json.Decode as D exposing (Decoder)
import Time exposing (Posix)


apiResponseDecoder : Decoder a -> Decoder a
apiResponseDecoder dataDecoder =
    D.field "data" dataDecoder


timestampDecoder : Decoder Posix
timestampDecoder =
    D.map Time.millisToPosix D.int

commentDecoder : Decoder Comment
commentDecoder = 
    D.map7 intoComment
        (D.field "id" D.string)
        (D.field "anonAuthorName" D.string)
        (D.field "body" D.string)
        (D.field "replyIds" <| D.list D.string)
        (D.field "isLeaf" D.bool)
        (D.field "votes" D.int)
        (D.field "createdAt" timestampDecoder)


commentTreeDecoder : Decoder CommentTree
commentTreeDecoder =
    D.map4 CommentTree
        (D.field "comments" <| D.dict commentDecoder)
        (D.field "topLevelComments" <| D.list D.string)
        (D.field "siteVerified" D.bool)
        (D.field "postId" D.string)

