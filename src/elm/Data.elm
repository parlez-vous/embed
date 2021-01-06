module Data exposing (token, tokenToString, Token, User(..), UserInfo, UserInfoWithToken)

import Time

type User
    = Authenticated UserInfo 
    -- We may have access to a server-generated
    -- random username if the user has already commented
    | Anonymous (Maybe String)

type alias UserInfo =
    { id : String
    , username : String
    , created : Time.Posix
    , updated : Time.Posix
    , isModerator : Bool
    }


type Token = Token String

token : String -> Token
token = Token

tokenToString : Token -> String
tokenToString (Token val) = val

type alias UserInfoWithToken = ( UserInfo, Token )

