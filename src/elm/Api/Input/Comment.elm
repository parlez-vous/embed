module Api.Input.Comment exposing (CommentTree, commentTreeListDecoder)

{-| Comment Related Decoders and Types
-}

import Json.Decode as D exposing (Decoder)


type alias CommentTree =
    CommentWithReplies 
        (CommentWithReplies
            LeafComment 
        )
   


{-
type alias WithReplies a =
    { replies : List a
    }

type alias Comment a =
    { a
        | id : String
        , anonymousAuthorName : String
    }
-}


type alias CommentWithReplies a =
    { id : String
    , anonymousAuthorName : String
    , replies : List a
    }
        
        
type alias LeafComment =
    { id : String
    , anonymousAuthorName : String
    }


    
-- intentionally ommiting type here
commonFields =
    { id = D.field "id" D.string
    , anonymousAuthorName = D.field "anon_author_name" D.string
    }


leafCommentDecoder : Decoder LeafComment
leafCommentDecoder =
    D.map2 LeafComment
        commonFields.id
        commonFields.anonymousAuthorName
        
    

makeCommentTreeDecoder : Decoder a -> Decoder (CommentWithReplies a)
makeCommentTreeDecoder nextDecoder =
    D.map3 CommentWithReplies
        commonFields.id
        commonFields.anonymousAuthorName
        (D.field "replies" <| D.list nextDecoder)


commentTreeListDecoder : Decoder (List CommentTree)
commentTreeListDecoder =
    let
        commentTreeDecoder =
            makeCommentTreeDecoder
                (makeCommentTreeDecoder leafCommentDecoder
                )
    in
    D.list commentTreeDecoder

