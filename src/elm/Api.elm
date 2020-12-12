module Api exposing
    ( Api
    , ApiClient
    , apiFactory
    , getApiClient
    )

import Api.Output as Output
import Data.Comment exposing (Comment, CommentTree)
import Data.Cuid exposing (Cuid)
import Http
import Api.Input as Input
import Json.Decode as D exposing (Decoder)
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)


type Api
    = Api Url



type alias ApiClient =
    { getPostComments : GetPostComments
    , getRepliesForComment : GetRepliesForComment
    , addComment : AddComment 
    }


apiFactory : Url -> Api
apiFactory =
    Api

getApiClient : Api -> ApiClient 
getApiClient api =
    { getPostComments = getPostComments api
    , getRepliesForComment = getRepliesForComment api
    , addComment = addComment api
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

{-

GET /embed/sites/test.com/posts/chaining-failable-tasks.html/comments HTTP/1.1
Host: localhost:3000
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:83.0) Gecko/20100101 Firefox/83.0
Accept: */*
Accept-Language: en-CA,en-US;q=0.7,en;q=0.3
Accept-Encoding: gzip, deflate
Origin: http://localhost:8080
Connection: keep-alive
Referer: http://localhost:8080/
Pragma: no-cache
Cache-Control: no-cache

-}

makeRequestUrl : Api -> String -> List QueryParameter -> String
makeRequestUrl (Api url) routePath queryParams =
    let
        stringifiedUrl =
            let
                raw =
                    Url.toString url
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


type alias GetPostComments = Task Http.Error CommentTree

getPostComments : Api -> GetPostComments 
getPostComments api =
    let
        endpointPath = "sites/test.com/posts/chaining-failable-tasks.html/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder
    in
    getTask
        (makeRequestUrl api endpointPath noParams)
        (requestResolver decoder)



type alias GetRepliesForComment =
    Cuid -> Task Http.Error CommentTree

    
getRepliesForComment : Api -> GetRepliesForComment
getRepliesForComment api commentId =
    let
        endpointPath = "sites/test.com/posts/chaining-failable-tasks.html/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder
            
        queryParams =
            [ Url.Builder.string "parentCommentId" commentId ]
    in
    getTask
        (makeRequestUrl api endpointPath queryParams)
        (requestResolver decoder)


type alias AddComment =
    String -> Cuid -> Maybe Cuid -> Task Http.Error Comment


addComment : Api -> AddComment
addComment api commentContents postId parentCommentId =
    let
        endpointPath = "posts/" ++ postId ++ "/comments"
        
        decoder =
            Input.apiResponseDecoder Input.commentDecoder

        body =
            Output.addCommentBody
                { body = commentContents
                , parentCommentId = parentCommentId
                , anonAuthorName = Nothing
                , authorId = Nothing
                }
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = makeRequestUrl api endpointPath noParams
        , body = body
        , resolver = requestResolver decoder
        , timeout = Nothing
        }
