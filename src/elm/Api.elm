module Api exposing
    ( Api
    , ApiClient
    , apiFactory
    , getApiClient
    )

import Api.Input as Input
import Api.Input.Comment as Comment exposing (Comment)
-- import Api.Output as Output
import Http
import Url exposing (Url)
import Url.Builder


type Api
    = Api Url



type alias ToMsg a msg =
    Result Http.Error a -> msg


type alias ApiClient msg =
    { getPostComments : GetPostComments msg
    }


apiFactory : Url -> Api
apiFactory =
    Api

getApiClient : Api -> ApiClient msg
getApiClient api =
    { getPostComments = getPostComments api
    }



makeRequestUrl : Api -> String -> String
makeRequestUrl (Api url) routePath =
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
        []




type alias GetPostComments msg =
    ToMsg Input.InitialCommentTree msg -> Cmd msg

getPostComments : Api -> GetPostComments msg
getPostComments api toMsg =
    let
        endpointPath = "sites/test.com/posts/chaining-failable-tasks.html/comments"

        decoder =
            Input.apiResponseDecoder Input.initialCommentsDecoder
            
        expect =
            Http.expectJson
                toMsg
                decoder
    in
    Http.get
        { url = makeRequestUrl api endpointPath
        , expect = expect
        }

