module Api.Input exposing
    ( apiResponseDecoder
    , Comment
    , CommentMap
    , CommentTree
    , commentDecoder
    , commentTreeDecoder
    , Cuid
    )

{-| Represents the Incoming data from the server.

Includes JSON decoders and types.
-}

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import RemoteData exposing (WebData)
import Time exposing (Posix)


apiResponseDecoder : Decoder a -> Decoder a
apiResponseDecoder dataDecoder =
    D.field "data" dataDecoder


type alias Cuid = String


type alias Comment =
    { id : String
    , anonymousAuthorName : String
    , body : String
    , replyIds : WebData (List Cuid)
    , votes : Int
    , createdAt : Posix
    }

type alias CommentMap = Dict Cuid Comment

type alias CommentTree =
    { comments : CommentMap
    , topLevelComments : List Cuid
    , siteVerified : Bool
    , postId : Cuid
    }


intoComment
    : String
    -> String
    -> String
    -> List String
    -> Bool
    -> Int
    -> Posix
    -> Comment
intoComment id anonAuthorName body replyIds isLeaf votes createdAt =
    let
        partialComment =
            Comment id anonAuthorName body

        commentWithReplyIds =
            if not isLeaf && List.length replyIds == 0 then
                partialComment (RemoteData.NotAsked) 
            else
                partialComment (RemoteData.Success replyIds)
    in
    commentWithReplyIds votes createdAt


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


commentMapDecoder : Decoder (CommentMap)
commentMapDecoder =
    D.dict commentDecoder

commentTreeDecoder : Decoder CommentTree
commentTreeDecoder =
    D.map4 CommentTree
        (D.field "comments" commentMapDecoder)
        (D.field "topLevelComments" (D.list D.string))
        (D.field "siteVerified" D.bool)
        (D.field "postId" D.string)

