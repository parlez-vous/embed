module Api exposing
    ( Api
    , ApiClient
    , apiFactory
    , getApiClient
    , reportError 
    )

import Api.Output as Output
import Data.Comment exposing (Comment, CommentTree)
import Data.Cuid exposing (Cuid)
import Http exposing (Body)
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
    }



apiFactory : Url -> Url -> Api
apiFactory =
    Api


getApiClient : Api -> ApiClient 
getApiClient api =
    { getPostComments = getPostComments api
    , getRepliesForComment = getRepliesForComment api
    , addComment = addComment api
    , reportError = reportError api
    }


noParams : List QueryParameter
noParams = []


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


getTask : String -> Http.Resolver Http.Error a -> Task Http.Error a
getTask url resolver =
    Http.task
        { method = "GET"
        , headers = []
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

        embedRootPath =
            "embed"

        routePathList =
            String.split "/" routePath

        pathComponents =
            embedRootPath :: routePathList
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
        
        endpointPath = "sites/" ++ hostname ++ "/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder

        queryParams =
            [ Url.Builder.string "postId" path ]
    in
    getTask
        (makeRequestUrl api endpointPath queryParams)
        (requestResolver decoder)



type alias GetRepliesForComment =
    Cuid -> Task Http.Error CommentTree

    
getRepliesForComment : Api -> GetRepliesForComment
getRepliesForComment api commentId =
    let
        { hostname, path } = getSiteInfo api
        
        endpointPath = "sites/" ++ hostname ++ "/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder
            
        queryParams =
            [ Url.Builder.string "parentCommentId" commentId
            , Url.Builder.string "postId" path
            ]
    in
    getTask
        (makeRequestUrl api endpointPath queryParams)
        (requestResolver decoder)


type alias AddComment =
    String -> Cuid -> Maybe Cuid -> Maybe String -> Task Http.Error Comment


addComment : Api -> AddComment
addComment api commentContents postId parentCommentId anonymousAuthorName =
    let
        endpointPath = "posts/" ++ postId ++ "/comments"
        
        decoder =
            Input.apiResponseDecoder Input.commentDecoder

        body =
            Output.addCommentBody
                { body = commentContents
                , parentCommentId = parentCommentId
                , anonAuthorName = anonymousAuthorName
                , authorId = Nothing
                }
    in
    postTask
        (makeRequestUrl api endpointPath noParams)
        body
        (requestResolver decoder)

