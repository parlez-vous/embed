module Data.Comment exposing
    ( Comment
    , CommentMap
    , CommentTree
    , addNewComment
    , isReply
    , updateComment
    , setComment
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


isReply : Comment -> Bool
isReply { parentCommentId } =
    case parentCommentId of
        Just _ -> True
        Nothing -> False




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


updateComment : (Comment -> Comment) -> Cuid -> CommentTree -> CommentTree
updateComment f commentCuid currentTree =
    let
        newComments =
            Dict.update commentCuid (Maybe.map f) currentTree.comments
    in
    { currentTree | comments = newComments }



setComment : Comment -> CommentTree -> CommentTree
setComment comment =
    updateComment (always comment) comment.id


addNewComment : Comment -> CommentTree -> CommentTree
addNewComment newComment commentTree =
    let
        addNewCommentToCommentMap : CommentTree -> CommentTree
        addNewCommentToCommentMap tree =
            { tree
                | comments =
                    Dict.insert newComment.id newComment tree.comments
            }

        addReply : Cuid -> CommentTree
        addReply parentCommentId = 
            let
                -- updates parent with the new comment's pointer
                updateParentComment =
                    updateComment
                        (\parentComment ->
                            { parentComment
                                | replyIds =
                                    Set.insert newComment.id parentComment.replyIds

                                -- if the parent was a leaf, it not longer will be
                                , isLeaf = False

                                -- reset the text area state
                                , textAreaState = ( False, "" )
                            }
                        )
                        parentCommentId

            in
            commentTree
            |> updateParentComment
            |> addNewCommentToCommentMap


        -- adding an empty tuple argument to enforce lazyness
        addTopLevelComment : () -> CommentTree
        addTopLevelComment _ =
            let
                addNewCommentPointerToTopLevelComments tree =
                    { tree 
                        | topLevelComments =
                            Set.insert newComment.id commentTree.topLevelComments
                    }

            in
            commentTree
            |> addNewCommentPointerToTopLevelComments
            |> addNewCommentToCommentMap

    in
    case newComment.parentCommentId of
        Just parentCommentId ->
            addReply parentCommentId

        Nothing ->
            addTopLevelComment ()

