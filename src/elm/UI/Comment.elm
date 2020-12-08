module UI.Comment exposing (viewCommentBox)

import Css exposing (..)
import Html exposing (Html)
import Html.Styled as S exposing (toUnstyled)
import Html.Styled.Attributes exposing (css)
import Api.Input.Comment exposing (Comment)
import Time
import RemoteData exposing (WebData)
import Utils exposing (humanReadableTimestamp)

type alias StyledHtml a = S.Html a

type alias TimeFormatter = Time.Posix -> String


viewSingleComment : TimeFormatter -> Comment -> StyledHtml msg
viewSingleComment formatter comment =
    let
        styles =
            [ marginBottom (px 15) ]

        timeStampInfo =
            S.span [] [ S.text <| formatter comment.createdAt ]

        authorName =
            S.span [ css [ color (hex "#737373") ] ]
                [ S.text comment.anonymousAuthorName
                , timeStampInfo
                ]

    in
    S.div [ css styles ]
        [ authorName
        , S.div [] [ S.text comment.body ] 
        ]

viewCommentBox : WebData (List Comment) -> TimeFormatter -> Html msg
viewCommentBox webDataComments formatter =
    toUnstyled <|
        case webDataComments of
            RemoteData.Success comments ->
                S.div [] (List.map (viewSingleComment formatter) comments)

            _ ->
                S.div [] [ S.text "?????" ]

