module UI.TextArea exposing (replyTextArea, topLevelTextArea, toHtml)


import Ant.Button as Btn exposing (button)
import Ant.Input as Input exposing (input, Input)
import Css exposing (..)
import Data.Comment exposing (Comment)
import Html.Styled as S exposing (Html, fromUnstyled)
import Html.Styled.Attributes exposing (css)


type TextArea msg = TextArea (Input msg, String, Maybe (Html msg))

replyTextArea : Comment -> ( String -> msg ) -> TextArea msg
replyTextArea comment updateTextArea =
    let
        textAreaInput = 
            input updateTextArea
                |> Input.withTextAreaType { rows = 4 } 
                |> Input.withPlaceholder ("respond to " ++ comment.anonymousAuthorName)
    in
    TextArea (textAreaInput, Tuple.second comment.textAreaState, Nothing)


topLevelTextArea : ( String -> msg ) -> String -> Html msg -> TextArea msg
topLevelTextArea updateTextArea textAreaValue extraInfo =
    let
        textAreaInput =
            input updateTextArea
                |> Input.withTextAreaType { rows = 5 }
                |> Input.withPlaceholder "What are your thoughts?"
    in
    TextArea (textAreaInput, textAreaValue, Just extraInfo)



toHtml : Maybe msg -> TextArea msg -> Html msg
toHtml maybeSubmit (TextArea (input, value, maybeExtraInfo)) =
    let
        htmlTextArea =
            Input.toHtml value input
            |> fromUnstyled

        noActionAvailable =
            case maybeSubmit of
                Just _ -> False
                Nothing -> True

        maybeBindOnClick btn =
            case maybeSubmit of
                Just submit ->
                    Btn.onClick submit btn

                Nothing ->
                    btn

        submitCommentButton =
            button "Add Comment"
            |> Btn.disabled (String.length value == 0 || noActionAvailable)
            |> maybeBindOnClick
            |> Btn.toHtml
            |> fromUnstyled

        extraInfo =
            case maybeExtraInfo of
                Just info -> info
                Nothing -> S.text ""

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
            [ extraInfo, submitCommentButton ]
        ]


