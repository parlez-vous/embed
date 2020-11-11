module Main exposing (main)

import Browser
import Html exposing (div, Html)
import Html.Attributes as Attr


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = Int


-- MSG

type alias Msg = Int


-- INIT

init : { origin: String } -> (Model, Cmd msg)
init flags =
    let
        _ = Debug.log "> " flags
    in
    (0, Cmd.none)


-- UPDATE

update : Msg -> Model -> ( Model, Cmd msg )
update _ _ = (0, Cmd.none)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html msg
view _ =
  div [ Attr.class "parlez-container" ]
    [ H.textarea [ Attr.placeholder "What are your thoughts?" ] []
    ]
