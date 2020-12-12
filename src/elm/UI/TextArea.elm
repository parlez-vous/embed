module UI.TextArea exposing (replyTextArea, topLevelTextArea, toHtml)


import Ant.Input as Input exposing (input, Input)
import Ant.Button as Btn exposing (button)
import Data.Comment exposing (Comment)
import Html.Styled as S exposing (Html, fromUnstyled)


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
topLevelTextArea updateTextArea =
    let
        textAreaInput =
            input updateTextArea
                |> Input.withTextAreaType { rows = 5 }
                |> Input.withPlaceholder "What are your thoughts?"
    in
    TextArea << Tuple.pair textAreaInput



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
    in
    S.div []
        [ htmlTextArea
        , submitCommentButton
        ]


