module UI.Comment exposing (viewCommentBox)

import Ant.Typography.Text as Text exposing (Text)
import Css exposing (..)
import Html exposing (Html)
import Html.Styled as S exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Api.Input.Comment exposing (Comment, Replies, getReplies)
import Time
import RemoteData exposing (WebData)


type alias StyledHtml a = S.Html a

type alias TimeFormatter = Time.Posix -> String


renderText : Text -> StyledHtml msg
renderText = Text.toHtml >> fromUnstyled


strongText : String -> StyledHtml msg
strongText val =
    Text.text val
    |> Text.strong
    |> renderText


primaryText : String -> StyledHtml msg
primaryText val =
    Text.text val
    |> Text.withType Text.Primary
    |> renderText



secondaryText : String -> StyledHtml msg
secondaryText val =
    Text.text val
    |> Text.withType Text.Secondary
    |> renderText


viewComments : TimeFormatter -> WebData (List Comment) -> StyledHtml msg
viewComments formatter webDataReplies =
    case webDataReplies of
        RemoteData.Success comments ->
            if List.length comments == 0 then
                S.div [] []
            else
                S.div
                    [ css [ marginLeft (px 15) ] ]
                    (List.map (viewSingleComment formatter) comments)

        RemoteData.NotAsked ->
            S.div [] [ S.text "load more comments" ]

        RemoteData.Failure _ ->
            S.div [] [ S.text "something went wrong" ]

        RemoteData.Loading ->
            S.div [] [ S.text "loading" ]


viewSingleComment : TimeFormatter -> Comment -> StyledHtml msg
viewSingleComment formatter comment =
    let
        styles =
            [ marginBottom (px 15)
            ]

        authorName =
            S.span [ css [ marginRight (px 10) ] ]
                [ strongText comment.anonymousAuthorName
                ]

        commentReplies = RemoteData.map getReplies comment.replies
    in
    S.div [ ]
        [ authorName
        , secondaryText <| formatter comment.createdAt
        , S.div [ css styles ] [ primaryText comment.body ]
        , viewComments formatter commentReplies
        ]


viewCommentBox : WebData (List Comment) -> TimeFormatter -> Html msg
viewCommentBox webDataComments formatter =
    toUnstyled <|
        S.div
            [ css [ marginLeft (px -15) ] ]
            [ viewComments formatter webDataComments ]

