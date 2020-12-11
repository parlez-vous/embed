module Api.Output exposing (addCommentBody)


{-| JSON encoders to send JSON data
-}

import Api.Input exposing (Cuid)
import Http exposing (Body)
import Json.Encode as E exposing (Value)
import Http



encodeNullable : (a -> Value) -> Maybe a -> Value
encodeNullable f =
    Maybe.map f
    >> Maybe.withDefault E.null  


type alias AddComment =
    { body : String
    , parentCommentId : Maybe Cuid
    , authorId : Maybe Cuid 
    , anonAuthorName : Maybe String
    }

addCommentBody : AddComment -> Body
addCommentBody { body, parentCommentId, authorId, anonAuthorName } =
    let
        maybeEncode = encodeNullable E.string

        jsonValue =
            E.object
                [ ( "body", E.string body )
                , ( "parentCommentId", maybeEncode parentCommentId )
                , ( "authorId", maybeEncode authorId )
                , ( "anonAuthorName", maybeEncode anonAuthorName )
                ]
    in
    Http.jsonBody jsonValue

