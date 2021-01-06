module UI.AuthenticationInfo exposing (viewAuthenticationInfo, logInForm, LogInValues, AuthenticationRequest(..))

import Ant.Form as Form exposing (Form)
import Ant.Form.PasswordField exposing (PasswordFieldValue)
import Ant.Theme as AntTheme
import Css exposing (..)
import Data exposing (User(..))
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Color.Convert exposing (colorToHexWithAlpha)

type AuthenticationRequest = SignUp | LogIn


type alias LogInValues =
    { usernameOrEmail : String
    , password : PasswordFieldValue
    }

{-
type alias SignUp =
    { username : String
    , email : String
    , password : String
    }
-}

logInForm : (String -> String -> msg) -> Form LogInValues msg
logInForm tagger =
    let
        usernameOrEmailField = 
            Form.inputField
                { parser = Ok
                , value = .usernameOrEmail
                , update = \new values -> { values | usernameOrEmail = new }
                , error = always Nothing
                , attributes =
                    { label = "Username or Email"
                    , placeholder = ""
                    }
                }

        passwordField = 
            Form.passwordField
                { parser = \{ value } -> Ok value
                , value = .password
                , update = \new values -> { values | password = new }
                , error = always Nothing
                , attributes =
                    { label = "Password"
                    , placeholder = ""
                    }
                }
    in
    Form.succeed tagger
    |> Form.append usernameOrEmailField
    |> Form.append passwordField



viewAuthenticationInfo : SimpleWebData User -> (AuthenticationRequest -> msg) -> Html msg
viewAuthenticationInfo webDataUser tagger =
    let
        createAuthButton textValue action =
            button
                [ onClick action
                , css
                    [ all initial
                    , fontFamily inherit
                    , cursor pointer
                    , color inherit
                    , hover
                        [ color <| hex <| colorToHexWithAlpha AntTheme.defaultColors.primaryFaded
                        ]
                    ]
                ]
                [ text textValue
                ]

        textColor =
            colorToHexWithAlpha AntTheme.defaultTheme.typography.secondaryTextColor
            |> hex
            |> color

        authenticationPrompt = 
            case webDataUser of
                SimpleWebData.Success user ->
                    case user of
                        Authenticated userInfo ->
                            [ text ("Logged in as " ++ userInfo.username)
                            ]

                        Anonymous _ ->
                            [ createAuthButton "Log in" LogIn
                            , text " or "
                            , createAuthButton "sign up" SignUp
                            , text " for a better experience."
                            ]

                SimpleWebData.Loading ->
                    [ text "Loading ..."
                    ]

                SimpleWebData.Failure _ ->
                    [ text "Something went wrong while verifying your session."
                    ]

        contents =
            div [ css [ textColor ] ] authenticationPrompt
    in
    Html.map tagger contents

