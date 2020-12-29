module UI.TextArea exposing (replyTextArea, topLevelTextArea, toHtml)


import Ant.Button as Btn exposing (button)
import Ant.Input as Input exposing (input, Input)
import Ant.Theme as AntTheme
import Color.Convert exposing (colorToHexWithAlpha)
import Css exposing (..)
import Data.Comment exposing (Comment)
import Html.Styled as S exposing (Html, fromUnstyled)
import Html.Styled.Attributes exposing (css)


type TextArea msg = TextArea (Input msg, String)

replyTextArea : Comment -> ( String -> msg ) -> TextArea msg
replyTextArea comment updateTextArea =
    let
        textAreaInput = 
            input updateTextArea
                |> Input.withTextAreaType { rows = 4 } 
                |> Input.withPlaceholder ("respond to " ++ comment.anonymousAuthorName)
    in
    TextArea ( textAreaInput, Tuple.second comment.textAreaState )


topLevelTextArea : ( String -> msg ) -> String -> TextArea msg
topLevelTextArea updateTextArea textAreaValue =
    let
        textAreaInput =
            input updateTextArea
                |> Input.withTextAreaType { rows = 5 }
                |> Input.withPlaceholder "What are your thoughts?"
    in
    TextArea (textAreaInput, textAreaValue)



toHtml : msg -> TextArea msg -> Html msg
toHtml submit (TextArea (input, value)) =
    let
        htmlTextArea =
            Input.toHtml value input
            |> fromUnstyled

        submitCommentButton =
            button "Add Comment"
            |> Btn.disabled (String.length value == 0)
            |> Btn.onClick submit
            |> Btn.toHtml
            |> fromUnstyled

        authenticationInfo =
            let
                createAuthButton text =
                    S.button
                        [ css
                            [ all initial
                            , fontFamily inherit
                            , cursor pointer
                            , color inherit
                            , hover
                                [ color <| hex <| colorToHexWithAlpha AntTheme.defaultColors.primaryFaded
                                ]
                            ]
                        ]
                        [ S.text text
                        ]

                textColor =
                    colorToHexWithAlpha AntTheme.defaultTheme.typography.secondaryTextColor
                    |> hex
                    |> color
            in
            S.div
                [ css [ textColor ] ]
                [ createAuthButton "Log in" 
                , S.text " or "
                , createAuthButton "sign up"
                , S.text " for a better experience."
                ]
    in
    S.div []
        [ htmlTextArea
        , S.div 
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                , marginTop (px 13)
                ]
            ]
            [ authenticationInfo, submitCommentButton ]
        ]


