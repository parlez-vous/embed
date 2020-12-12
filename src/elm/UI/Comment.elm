module UI.Comment exposing (viewCommentsSection)

{-| UI modules for rendering a tree of comments.
-}

import Ant.Button as Btn exposing (button, Button)
import Ant.Input exposing (input)
import Ant.Typography.Text as Text exposing (Text, text)
import Css exposing (..)
import Data.Comment exposing (Comment, CommentTree, CommentMap)
import Data.Cuid exposing (Cuid)
import Html.Styled as S exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Time
import RemoteData exposing (WebData)
import UI.TextArea as TextArea 
import Utils

type alias StyledHtml a = S.Html a
type alias TimeFormatter = Time.Posix -> String

type alias Effects msg =
    { loadRepliesForComment : Cuid -> msg
    , updateComment : Comment -> msg
    , submitReply : Cuid -> String -> msg
    }

type CommentPointers msg
    = Simple (List Cuid)
    | Async (WebData (List Cuid), msg)



renderText : Text -> StyledHtml msg
renderText = Text.toHtml >> fromUnstyled


strongText : String -> StyledHtml msg
strongText val =
    text val
    |> Text.strong
    |> renderText


primaryText : String -> StyledHtml msg
primaryText val =
    text val
    |> Text.withType Text.Primary
    |> renderText



secondaryText : String -> StyledHtml msg
secondaryText val =
    text val
    |> Text.withType Text.Secondary
    |> renderText



link : String -> Button msg
link val =
    button val
    |> Btn.withType Btn.Link


commentActionButton : String -> msg -> StyledHtml msg
commentActionButton value msg =
    S.button
        [ css
            [ border zero
            , paddingLeft zero
            , fontSize (px 11)
            ]
        , onClick msg
        ]
        [ S.text value ]




replyTextarea : Comment -> Effects msg -> StyledHtml msg
replyTextarea comment effects =
    let
        ( textAreaVisible, textAreaValue ) = comment.textAreaState

        updateTextArea =
            \val ->
                effects.updateComment
                    { comment | textAreaState =
                        Tuple.mapSecond (always val) comment.textAreaState
                    }

        textArea =
            TextArea.replyTextArea comment updateTextArea
            |> TextArea.toHtml (effects.submitReply comment.id textAreaValue)
    in
    if textAreaVisible then
        textArea
    else 
        S.text ""


{-| 
    @arg formatter
        a function that formats Posix timestamps into human readable "distance" strings.
        i.e. "5 minutes ago"

    @arg pointers
        cuid pointers that represent the current level of comments to be rendered in a recursive
        tree of comments. Can be Simple, i.e. they are immediately loaded, or Async, meaning we have to do
        a subsequent round-trip to the back end to get this information.

    @arg commentTree
        a flattened hashmap that represents a recursive tree of comments (i.e. just like Reddit)
-}
viewComments : Effects msg -> TimeFormatter -> CommentPointers msg -> CommentMap -> StyledHtml msg
viewComments effects formatter pointers commentMap =
    let
        viewComments_ : List Cuid -> StyledHtml msg
        viewComments_ pointerList =
            if List.length pointerList == 0 then
                S.text ""
            else
                let
                    comments =
                        Utils.getCommentsFromPointers commentMap pointerList
                in
                S.div
                    [ css [ marginLeft (px 15) ]
                    ]
                    (List.map viewSingleComment comments)


        -- this is embedded inside of view comments to
        -- take advantage of closures
        viewSingleComment : Comment -> StyledHtml msg
        viewSingleComment comment =
            let
                styles =
                    [ marginBottom (px 15)
                    ]

                authorName =
                    S.span [ css [ marginRight (px 10) ] ]
                        [ strongText comment.anonymousAuthorName
                        ]
                
                replyInfo =
                    Async
                        ( comment.replyIds
                        , effects.loadRepliesForComment comment.id
                        )

                replyButton =
                    let
                        update =
                            effects.updateComment
                                { comment | textAreaState =
                                    Tuple.mapFirst not comment.textAreaState
                                }
                    in
                    commentActionButton "reply" update

            in
            S.div []
                [ authorName
                , secondaryText <| formatter comment.createdAt
                , S.div [ css styles ]
                    [ S.div [] [ primaryText comment.body ]
                    , replyButton
                    , replyTextarea comment effects
                    ]
                , viewComments effects formatter replyInfo commentMap
                ]
    in
    case pointers of
        Simple pointerList ->
            viewComments_ pointerList

        Async ( webDataList, fetchReplies ) ->
            case webDataList of
                RemoteData.NotAsked ->
                    let
                        loadMoreBtn =
                            link "load more comments"
                            |> Btn.onClick fetchReplies
                            |> Btn.toHtml
                            |> fromUnstyled
                    in
                    S.div [] [ loadMoreBtn ]

                RemoteData.Loading ->
                    let
                        disabledLoadingButton =
                            link "loading..."
                            |> Btn.disabled True
                            |> Btn.toHtml
                            |> fromUnstyled
                    in
                    S.div [] [ disabledLoadingButton ]

                RemoteData.Failure e -> 
                    S.div [] [ S.text "error loading more comments" ]

                RemoteData.Success replyPointers ->
                    viewComments_ replyPointers



viewCommentsSection : Effects msg -> TimeFormatter -> CommentTree -> StyledHtml msg
viewCommentsSection effects formatter { topLevelComments, comments }=
    S.div
        [ css [ marginLeft (px -15) ] ]
        [ viewComments effects formatter (Simple topLevelComments) comments ]

