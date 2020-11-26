module Main exposing (main)

import Ant.Css
import Api
import Browser
import Html exposing (div, Html)
import Html.Attributes as Attr
import Css.ModernNormalize as NormalizeCss
import ParlezVousEmbed
import Url


type alias Flags = 
    { apiEndpoint : String
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
    | Failed String


-- MSG

type Msg = AppMsg ParlezVousEmbed.Msg


-- INIT

init : Flags -> (Model, Cmd Msg)
init flags =
    case Url.fromString flags.apiEndpoint of
        Nothing ->
            ( Failed <| "invalid api endpoint url: " ++ flags.apiEndpoint
            , Cmd.none
            )

        Just url ->
            let
                api = Api.apiFactory url

                ( embedModel, embedCmd ) = ParlezVousEmbed.init api
            in
            ( Ready embedModel, Cmd.map AppMsg embedCmd )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg )
update (AppMsg appMsg) model =
    case model of
        Failed reason ->
            ( Failed reason, Cmd.none )

        Ready embedModel ->
            ( Ready <| ParlezVousEmbed.update appMsg embedModel 
            , Cmd.none
            )



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    let
        contents =
            case model of
                Failed reason ->
                    Html.text reason

                Ready embedModel ->
                    ParlezVousEmbed.viewApp embedModel
                    |> Html.map AppMsg

    in
    div [ Attr.class "parlez-container" ]
        [ NormalizeCss.globalHtml
        , Ant.Css.defaultStyles
        , contents
        ]

