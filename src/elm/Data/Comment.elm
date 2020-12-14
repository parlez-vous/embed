module Data.Comment exposing
    ( Comment
    , CommentMap
    , CommentTree
    , updateComment
    , getCommentsFromPointers
    )

import Data.Cuid exposing (Cuid)
import Dict exposing (Dict)
import Time exposing (Posix)
import RemoteData exposing (WebData)
import Set exposing (Set)


type alias TextAreaVisibility = Bool

type alias Comment =
    { id : String
    , isLeaf : Bool
    , parentCommentId : Maybe Cuid
    , anonymousAuthorName : String
    , body : String

    -- CONTEXT REGARDING replyIds + remoteReplyBuffer:
    --
    -- Comment replies could be in one of the following states:
    -- 1. Comment came with all of its children replies 
    -- 2. Comment did not come with all of its replies (because it's so deeply nested in a comment tree)
    -- 3. Comment did not come with all of its replies AND we just added a new reply
    --      Hence you have a "partial" representation of all the replies of that comment
    --
    -- The buffer is used to represent the state of remote comments that we are fetching
    -- hence there is nothing store in the buffer because on RemoteData.Success we take
    -- the ids and add them to replyIds immediately
    , replyIds : Set Cuid
    , remoteReplyBuffer : WebData ()

    , votes : Int
    , createdAt : Posix
    , textAreaState : ( TextAreaVisibility, String )
    }


type alias CommentMap = Dict Cuid Comment

type alias CommentTree =
    { comments : CommentMap
    , topLevelComments : Set Cuid
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



getCommentsFromPointers : CommentMap -> Set Cuid -> List Comment
getCommentsFromPointers commentMap =
    Set.foldl
        (\cuid comments ->
            case Dict.get cuid commentMap of
                Just comment ->
                    comment :: comments
                Nothing ->
                    comments
        )
        []

