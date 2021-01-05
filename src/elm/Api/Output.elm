module Api.Output exposing (addCommentBody, LogIn)


{-| JSON encoders to send JSON data
-}

import Data.Cuid exposing (Cuid)
import Http exposing (Body)
import Json.Encode as E exposing (Value)



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

type alias LogIn =
    { usernameOrEmail : String 
    , password : String
    }

