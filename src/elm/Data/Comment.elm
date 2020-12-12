module Data.Comment exposing
    ( Comment
    , CommentMap
    , CommentTree
    , intoComment
    , updateComment
    )

import Data.Cuid exposing (Cuid)
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import Time exposing (Posix)


type alias TextAreaVisibility = Bool

type alias Comment =
    { id : String
    , anonymousAuthorName : String
    , body : String
    , replyIds : WebData (List Cuid)
    , votes : Int
    , createdAt : Posix
    , textAreaState : ( TextAreaVisibility, String )
    }


type alias CommentMap = Dict Cuid Comment

type alias CommentTree =
    { comments : CommentMap
    , topLevelComments : List Cuid
    , siteVerified : Bool
    , postId : Cuid
    }



updateComment : (Comment -> Comment) -> Cuid -> CommentTree -> CommentTree
updateComment f commentCuid currentTree =
    let
        newComments =
            Dict.update commentCuid (Maybe.map f) currentTree.comments
    in
    { currentTree | comments = newComments }



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

        textAreaVisibility = False

        textAreaState = ( textAreaVisibility, "" )

        commentWithReplyIds =
            if not isLeaf && List.length replyIds == 0 then
                partialComment RemoteData.NotAsked
            else
                partialComment (RemoteData.Success replyIds)
    in
    commentWithReplyIds votes createdAt textAreaState

