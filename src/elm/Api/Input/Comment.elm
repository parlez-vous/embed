module Api.Input.Comment exposing (Comment, commentTreeListDecoder)

{-| Comment Related Decoders and Types
-}

import Json.Decode as D exposing (Decoder)
import Time exposing (Posix)


type alias Comment =
    { id : String
    , anonymousAuthorName : String
    , body : String
    , replies : Maybe Replies
    , votes : Int
    , createdAt : Posix
    }

type Replies = Replies (List Comment)


timestampDecoder : Decoder Posix
timestampDecoder =
    D.map Time.millisToPosix D.int



commentTreeDecoder : Decoder Comment
commentTreeDecoder =
    D.map6 Comment
        (D.field "id" D.string)
        (D.field "anon_author_name" D.string)
        (D.field "body" D.string)
        (D.field "replies" repliesDecoder)
        (D.field "votes" D.int)
        (D.field "created_at" timestampDecoder)


repliesDecoder : Decoder (Maybe Replies)
repliesDecoder =
    D.lazy (\_ -> D.list commentTreeDecoder)
    |> D.map Replies
    |> D.maybe
    


commentTreeListDecoder : Decoder (List Comment)
commentTreeListDecoder =
    D.list commentTreeDecoder 

