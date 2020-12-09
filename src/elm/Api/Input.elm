module Api.Input exposing (apiResponseDecoder, InitialCommentTree, initialCommentsDecoder)

{-| Represents the Incoming data from the server.

Includes JSON decoders and types.
-}

import Api.Input.Comment exposing (Comment, commentTreeListDecoder)
import Json.Decode as D exposing (Decoder, Value)


apiResponseDecoder : Decoder a -> Decoder a
apiResponseDecoder dataDecoder =
    D.field "data" dataDecoder


{- Might need this in the future

withSessionToken : Decoder a -> (a, Decoder String)
withSessionToken dataDecoder =
    D.map2 Tuple.pair
        (apiResponseDecoder dataDecoder)
        (D.field "session_token" D.string)


-}


type alias InitialCommentResponse =
    { comments : Value --> Opaque value. Need to assess the leafIds before parsing.
    , leafIds : List String
    , siteVerified : Bool
    , postId : String
    }

type alias InitialCommentTree =
    { comments : List Comment
    , leafIds : List String
    , siteVerified : Bool
    , postId : String
    }



intoCommentTree : InitialCommentResponse -> Decoder InitialCommentTree
intoCommentTree { leafIds, siteVerified, postId } =
    D.map4 InitialCommentTree
        (D.field "comments" <| commentTreeListDecoder leafIds)
        (D.field "leafIds" <| D.succeed leafIds)
        (D.field "siteVerified" <| D.succeed siteVerified)
        (D.field "postId" <| D.succeed postId)
        

initialCommentsDecoder : Decoder InitialCommentTree
initialCommentsDecoder =
    let
        rawDecoder : Decoder InitialCommentResponse
        rawDecoder =
            D.map4 InitialCommentResponse
                (D.field "comments" D.value)
                (D.field "leafIds" (D.list D.string))
                (D.field "siteVerified" D.bool)
                (D.field "postId" D.string)
    in
    rawDecoder
        |> D.andThen intoCommentTree

