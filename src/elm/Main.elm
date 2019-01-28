module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


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

init : () -> (Model, Cmd msg)
init _ = (0, Cmd.none)


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
  div [ class "parlez-container" ]
    [ textarea [] []
    ]
