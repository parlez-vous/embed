module Api exposing
    ( Api
    , ApiClient
    , apiFactory
    , getApiClient
    )

import Api.Input as Input exposing (Cuid)
import Api.Output as Output

-- import Api.Output as Output
import Http
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)


type Api
    = Api Url



type alias ToMsg a msg =
    Result Http.Error a -> msg


type alias ApiClient msg =
    { getPostComments : GetPostComments msg
    , getRepliesForComment : GetRepliesForComment msg
    , addComment : AddComment msg
    }


apiFactory : Url -> Api
apiFactory =
    Api

getApiClient : Api -> ApiClient msg
getApiClient api =
    { getPostComments = getPostComments api
    , getRepliesForComment = getRepliesForComment api
    , addComment = addComment api
    }


noParams : List QueryParameter
noParams = []

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




type alias GetPostComments msg =
    ToMsg Input.CommentTree msg -> Cmd msg

getPostComments : Api -> GetPostComments msg
getPostComments api toMsg =
    let
        endpointPath = "sites/test.com/posts/chaining-failable-tasks.html/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder
            
        expect =
            Http.expectJson
                toMsg
                decoder
    in
    Http.get
        { url = makeRequestUrl api endpointPath noParams 
        , expect = expect
        }


type alias GetRepliesForComment msg =
    Cuid -> ToMsg Input.CommentTree msg -> Cmd msg

    
getRepliesForComment : Api -> GetRepliesForComment msg
getRepliesForComment api commentId toMsg =
    let
        endpointPath = "sites/test.com/posts/chaining-failable-tasks.html/comments"

        decoder =
            Input.apiResponseDecoder Input.commentTreeDecoder
            
        expect =
            Http.expectJson
                toMsg
                decoder

        queryParams =
            [ Url.Builder.string "parentCommentId" commentId ]
    in
    Http.get
        { url = makeRequestUrl api endpointPath queryParams 
        , expect = expect
        }


type alias AddComment msg =
    String -> Cuid -> Maybe Cuid -> ToMsg Input.Comment msg -> Cmd msg


addComment : Api -> AddComment msg
addComment api commentContents postId parentCommentId toMsg =
    let
        endpointPath = "posts/" ++ postId ++ "/comments"
        
        decoder =
            Input.apiResponseDecoder Input.commentDecoder

        expect =
            Http.expectJson
                toMsg
                decoder
        
        body =
            Output.addCommentBody
                { body = commentContents
                , parentCommentId = parentCommentId
                , anonAuthorName = Nothing
                , authorId = Nothing
                }

    in
    Http.post
        { url = makeRequestUrl api endpointPath noParams
        , body = body
        , expect = expect
        }
