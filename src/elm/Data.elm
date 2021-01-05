module Data exposing (token, tokenToString, Token, User, UserWithToken)

import Time

type alias User =
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

type alias UserWithToken = ( User, Token )

