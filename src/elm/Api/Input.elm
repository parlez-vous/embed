module Api.Input exposing (apiResponseDecoder, InitialCommentResponse, initialCommentsDecoder)

{-| Represents the Incoming data from the server.

Includes JSON decoders and types.
-}

import Api.Input.Comment exposing (CommentTree, commentTreeListDecoder)
import Json.Decode as D exposing (Decoder)


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
    { comments : List CommentTree
    , leafIds : List String
    , siteVerified : Bool
    , postId : String
    }


type alias User =
    { id : String
    , username : String
    }


commentAuthorDecoder : Decoder User
commentAuthorDecoder =
    D.map2 User
        D.string
        D.string


initialCommentsDecoder : Decoder InitialCommentResponse
initialCommentsDecoder =
    D.map4 InitialCommentResponse
        (D.field "comments" commentTreeListDecoder)
        (D.field "leafIds" (D.list D.string))
        (D.field "siteVerified" D.bool)
        (D.field "postId" D.string)
        
