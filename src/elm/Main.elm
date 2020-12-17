module Main exposing (main)

import Ant.Css
import Api exposing (Api)
import Browser
import Css exposing (..)
import Css.Media as Media exposing (withMedia)
import Css.ModernNormalize as NormalizeCss
import Html exposing (Html)
import Html.Styled as Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes as Attr exposing (css)
import ParlezVousEmbed
import Task
import Time
import Url


type alias Flags = 
    { apiEndpoint : String
    , siteUrl : String
    , anonymousUsername : Maybe String
    }


main : Program Flags Model Msg 
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Model
    = Ready ParlezVousEmbed.Model
    | NotReady Api (Maybe String)
    | Failed String


-- MSG

type Msg
    = AppMsg ParlezVousEmbed.Msg
    | NewCurrentTime Time.Posix


-- INIT

init : Flags -> (Model, Cmd Msg)
init flags =
    case ( Url.fromString flags.apiEndpoint, Url.fromString flags.siteUrl ) of
        ( Just apiBaseUrl, Just siteUrl ) ->
            let
                api = Api.apiFactory apiBaseUrl siteUrl
            in
            ( NotReady api flags.anonymousUsername, Task.perform NewCurrentTime Time.now)

        _ ->
            ( Failed <| "invalid api endpoint or site url: " ++ "(" ++ flags.apiEndpoint ++ ", " ++ flags.siteUrl ++ ")"
            , Cmd.none
            )



-- UPDATE

updateReadyModel : Msg -> ParlezVousEmbed.Model -> ( Model, Cmd Msg )
updateReadyModel msg embedModel =
    case msg of
        AppMsg appMsg -> 
            let
                ( newEmbedModel, embedCmd ) = ParlezVousEmbed.update appMsg embedModel
            in
            ( Ready newEmbedModel 
            , Cmd.map AppMsg embedCmd
            )

        NewCurrentTime time ->
            ( Ready <| ParlezVousEmbed.setCurrentTime time embedModel
            , Cmd.none 
            )
            

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Failed reason, _ ) ->
            ( Failed reason, Cmd.none )

        ( NotReady api maybeAnonymousUsername, NewCurrentTime time ) ->
            let
                ( embedModel, embedCmd ) = ParlezVousEmbed.init maybeAnonymousUsername api time
            in
            ( Ready embedModel, Cmd.map AppMsg embedCmd )

        ( NotReady api maybeAnonymousUsername, _ ) ->
            ( NotReady api maybeAnonymousUsername, Cmd.none )


        ( Ready embedModel, _ ) ->
            updateReadyModel msg embedModel




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        fiveMinutes = 1000 * 60 * 5
    in
    Time.every fiveMinutes NewCurrentTime 


-- VIEW


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

view : Model -> Html Msg
view model =
    let
        contents =
            case model of
                Failed reason ->
                    Styled.text reason

                NotReady _ _ ->
                    Styled.div [] []

                Ready embedModel ->
                    ParlezVousEmbed.viewApp embedModel
                    |> Styled.map AppMsg

    in
    toUnstyled <|
        Styled.div
            [ Attr.class "parlez-container"
            , css
                [ maxWidth (px 800)
                , marginRight auto
                , marginLeft auto
                , mediaQueries.extraSmall
                , mediaQueries.small
                , mediaQueries.medium
                , mediaQueries.large
                ]
            ]
            [ fromUnstyled NormalizeCss.globalHtml
            , fromUnstyled Ant.Css.defaultStyles
            , contents
            ]

