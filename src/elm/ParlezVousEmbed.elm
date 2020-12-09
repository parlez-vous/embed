module ParlezVousEmbed exposing (init, viewApp, Model, Msg, setCurrentTime, update)

import Ant.Input as Input exposing (input)
import Api exposing (Api)
import Api.Input as ApiInput
import Api.Input.Comment exposing (Comment)
import Html exposing (Html, div)
import Css exposing (Style, auto, marginRight, marginLeft, maxWidth, pct, px)
import Css.Media as Media exposing (withMedia)
import Html.Styled as Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Http
import RemoteData exposing (WebData)
import Time
import UI.Comment exposing (viewCommentBox)
import Utils exposing (humanReadableTimestamp)


{-
    1. Server sends us a partial tree
    2. We walk the tree and see whether the tree is "complete" or not
        i.e. are there even more comments deeply nested in the tree that requires us to do a
        subsequent round trip to the server
-}


type alias Model =
    { textAreaValue : String
    , comments : WebData (List Comment)
    , currentTime : Time.Posix
    }


type Msg
    = TextAreaValueChanged String
    | InitialPostCommentsFetched (Result Http.Error ApiInput.InitialCommentTree)

init : Api -> Time.Posix -> ( Model, Cmd Msg )
init api time =
    let
        initialModel =
            { textAreaValue = ""
            , comments = RemoteData.Loading
            , currentTime = time
            }

        apiClient = Api.getApiClient api
    in
    (initialModel, apiClient.getPostComments InitialPostCommentsFetched)


setCurrentTime : Time.Posix -> Model -> Model
setCurrentTime time model =
    { model | currentTime = time
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextAreaValueChanged newValue ->
            { model | textAreaValue = newValue }

        InitialPostCommentsFetched httpRequestResult ->
            case httpRequestResult of
                Err e ->
                    model
                Ok initialCommentResponse ->
                    let
                        _ = Debug.log "> initial response: " initialCommentResponse
                    in
                    { model | comments =
                        RemoteData.Success initialCommentResponse.comments
                    }





------------------------------------------
------------------------------------------
------------------------------------------
-- View

type alias MediaQueries =
    { extraSmall : Style
    , small : Style
    , medium : Style
    , large : Style
    }


extraSmallMediaQueries : Style
extraSmallMediaQueries =
    withMedia [ Media.only Media.screen [ Media.maxWidth (px 375) ] ]
        [ maxWidth (pct 95)
        ]


smallMediaQueries : Style
smallMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 376), Media.maxWidth (px 640) ] ]
        [ maxWidth (pct 94)
        ]


mediumMediaQueries : Style
mediumMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 641), Media.maxWidth (px 1007) ] ]
        [ maxWidth (px 600)
        ]



largeMediaQueries : Style
largeMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 1008) ] ]
        [ maxWidth (px 800)
        ]



mediaQueries : MediaQueries
mediaQueries =
    { extraSmall = extraSmallMediaQueries
    , small = smallMediaQueries
    , medium = mediumMediaQueries
    , large = largeMediaQueries
    }



viewApp : Model -> Html Msg
viewApp model =
    let
        textArea =
            input TextAreaValueChanged
            |> Input.withTextAreaType { rows = 5 }
            |> Input.withPlaceholder "What are your thoughts?"
            |> Input.toHtml model.textAreaValue

        timeStampFormatter = humanReadableTimestamp model.currentTime

        commentBox =
            viewCommentBox model.comments timeStampFormatter

        contents =
            [ textArea, commentBox ]
            |> List.map fromUnstyled

        styledAppShell =
            Styled.div
                [ css
                    [ maxWidth (px 800)
                    , marginRight auto
                    , marginLeft auto
                    , mediaQueries.extraSmall
                    , mediaQueries.small
                    , mediaQueries.medium
                    , mediaQueries.large
                    ]
                ]
                contents
    in
    toUnstyled styledAppShell 

