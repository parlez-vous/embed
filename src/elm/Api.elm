module Api exposing
    ( ApiClient
    , getApiClient
    , reportError 
    )

import Api.Output as Output
import Data exposing (User(..), UserInfo, UserInfoWithToken)
import Data.Comment exposing (Comment, CommentTree)
import Data.Cuid exposing (Cuid)
import Http exposing (Body, Header)
import Api.Input as Input
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Utils


type alias Api =
    { baseUrl : Url
    , siteUrl : Url
    }



type alias ApiClient =
    { getPostComments : GetPostComments
    , getRepliesForComment : GetRepliesForComment
    , addComment : AddComment 
    , reportError : ReportError
    , userLogIn : LogIn
    , userSignUp : SignUp
    , getUserFromSessionToken : GetUserFromSessionToken
    }



getApiClient : Url -> Url -> ApiClient 
getApiClient baseUrl siteUrl =
    let
        api = Api baseUrl siteUrl
    in
    { getPostComments = getPostComments api
    , getRepliesForComment = getRepliesForComment api
    , addComment = addComment api
    , reportError = reportError api
    , userLogIn = userLogIn api
    , userSignUp = userSignUp api
    , getUserFromSessionToken = getUserFromSessionToken api
    }


noParams : List QueryParameter
noParams = []


noHeaders : List Header
noHeaders = []



requestResolver : Decoder a -> Http.Resolver Http.Error a
requestResolver decoder =
    Http.stringResolver (\response ->
        case response of
            Http.BadUrl_ url ->
              Err (Http.BadUrl url)

            Http.Timeout_ ->
              Err Http.Timeout

            Http.NetworkError_ ->
              Err Http.NetworkError

            Http.BadStatus_ metadata _ ->
              Err (Http.BadStatus metadata.statusCode)

            Http.GoodStatus_ _ body ->
              case D.decodeString decoder body of
                Ok value ->
                  Ok value

                Err err ->
                  Err (Http.BadBody (D.errorToString err))
        )


getTask : String -> List Header -> Http.Resolver Http.Error a -> Task Http.Error a
getTask url headers resolver =
    Http.task
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , resolver = resolver
        , timeout = Nothing
        }



postTask : String -> Body -> Http.Resolver Http.Error a -> Task Http.Error a
postTask url body resolver =
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , resolver = resolver
        , timeout = Nothing
        }


makeRequestUrl : Api -> String -> List QueryParameter -> String
makeRequestUrl { baseUrl } routePath queryParams =
    let
        stringifiedUrl =
            let
                raw =
                    Url.toString baseUrl
            in
            if String.endsWith "/" raw then
                String.dropRight 1 raw

            else
                raw


        pathComponents =
            if routePath == "/error-reporting" then
                [ String.dropLeft 1 routePath ]
            else
                String.split "/" routePath

    in
    Url.Builder.crossOrigin
        stringifiedUrl
        pathComponents
        queryParams


type alias SiteInfo =
    { hostname : String
    , path : String
    }


getSiteInfo : Api -> SiteInfo
getSiteInfo { siteUrl} =
    { hostname = siteUrl.host
    , path = Utils.getPathFromUrl siteUrl
    }







-- Actual API Requests

type alias ErrorInfo =
    { ref : String
    , message : String
    }

type alias ReportError = ErrorInfo -> Task Http.Error ()

reportError : Api -> ReportError
reportError api { ref, message } =
    let
        endpointPath = "/error-reporting"

        body =
            E.object
                [ ( "ref", E.string ref)
                , ( "errorMessage", E.string message )
                ]
    in
    postTask
        (makeRequestUrl api endpointPath noParams)
        (Http.jsonBody body)
        (requestResolver <| D.succeed ())




type alias GetPostComments = Task Http.Error CommentTree

getPostComments : Api -> GetPostComments 
getPostComments api =
    let
        { hostname, path } = getSiteInfo api
        
        endpointPath = "embed/sites/" ++ hostname ++ "/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder

        queryParams =
            [ Url.Builder.string "postId" path ]
    in
    getTask
        (makeRequestUrl api endpointPath queryParams)
        noHeaders
        (requestResolver decoder)



type alias GetRepliesForComment =
    Cuid -> Task Http.Error CommentTree

    
getRepliesForComment : Api -> GetRepliesForComment
getRepliesForComment api commentId =
    let
        { hostname, path } = getSiteInfo api
        
        endpointPath = "embed/sites/" ++ hostname ++ "/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder
            
        queryParams =
            [ Url.Builder.string "parentCommentId" commentId
            , Url.Builder.string "postId" path
            ]
    in
    getTask
        (makeRequestUrl api endpointPath queryParams)
        noHeaders
        (requestResolver decoder)


type alias AddComment =
    String -> Cuid -> Maybe Cuid -> User -> Task Http.Error Comment


addComment : Api -> AddComment
addComment api commentContents postId parentCommentId user =
    let
        endpointPath = "embed/posts/" ++ postId ++ "/comments"
        
        decoder =
            Input.apiResponseDecoder Input.commentDecoder

        ( authorId, anonAuthorName ) =
            case user of
                Authenticated user_ ->
                    ( Just user_.id, Nothing )

                Anonymous maybeAnonymousUsername ->
                    ( Nothing, maybeAnonymousUsername )

        body =
            Output.addCommentBody
                { body = commentContents
                , parentCommentId = parentCommentId
                , anonAuthorName = anonAuthorName
                , authorId = authorId
                }
    in
    postTask
        (makeRequestUrl api endpointPath noParams)
        body
        (requestResolver decoder)



type alias LogIn =
    Output.LogIn -> Task Http.Error UserInfoWithToken


userLogIn : Api -> LogIn 
userLogIn api data =
    let
        endpointPath = "common/signin"

        signinJson =
            E.object
                [ ( "usernameOrEmail", E.string data.usernameOrEmail )
                , ( "password", E.string data.password )
                ]
    in
    postTask
        (makeRequestUrl api endpointPath noParams)
        (Http.jsonBody signinJson)
        (requestResolver Input.userAndTokenDecoder)


type alias SignUp =
    Output.SignUp -> Task Http.Error UserInfoWithToken


userSignUp : Api -> SignUp
userSignUp api data =
    let
        endpointPath = "common/signup"

        signUpJson =
            E.object
                [ ( "username", E.string data.username )
                , ( "email", E.string data.email )
                , ( "password", E.string data.password )
                ]
    in
    postTask
        (makeRequestUrl api endpointPath noParams)
        (Http.jsonBody signUpJson)
        (requestResolver Input.userAndTokenDecoder)

type alias GetUserFromSessionToken =
    String -> Task Http.Error UserInfo


getUserFromSessionToken : Api -> GetUserFromSessionToken
getUserFromSessionToken api token =
    let
        endpointPath = "common/profile"

        headers =
            [ Http.header "Authorization" token
            ]

        decoder =
            Input.apiResponseDecoder Input.userInfoDecoder
    in
    getTask
        (makeRequestUrl api endpointPath noParams)
        (headers)
        (requestResolver decoder)

