module Data exposing
    ( ApiToken(..)
    , Author(..)
    , User(..)
    , UserInfo
    , UserInfoWithToken
    , VoteType(..)
    , Interactions
    , CommentVote
    )

import Data.Cuid exposing (Cuid)
import Dict exposing (Dict)
import Time

type ApiToken = ApiToken String


type User
    = Authenticated UserInfo Interactions
    -- We may have access to a server-generated
    -- random username if the user has already commented
    | Anonymous (Maybe String)


type Author
    = Authenticated_ UserInfo
    | Anonymous_ String


type alias UserInfo =
    { id : String
    , username : String
    , created : Time.Posix
    , updated : Time.Posix
    , isModerator : Bool
    }


type VoteType = Up | Down 

type alias CommentVote =
    { value : Int -- either '-1' '0' or '1'
    , commentId : Cuid
    }


-- data about a particular action that a user has taken 

type alias Interactions =
    { commentVotes : Dict Cuid CommentVote
    }

type alias UserInfoWithToken = ( UserInfo, ApiToken )

