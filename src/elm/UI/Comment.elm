module UI.Comment exposing (viewCommentsSection)

{-| UI modules for rendering a tree of comments.
-}

import Ant.Button as Btn exposing (button, Button)
import Ant.Typography.Text as Text exposing (Text, text)
import Api.Input exposing (Comment, CommentTree, CommentMap, Cuid)
import Css exposing (..)
import Html.Styled as S exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Time
import RemoteData exposing (WebData)
import Dict

type alias StyledHtml a = S.Html a

type alias TimeFormatter = Time.Posix -> String

type alias FetchReplies msg = Cuid -> msg


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



viewSingleComment : FetchReplies msg -> TimeFormatter -> CommentMap -> Comment -> StyledHtml msg
viewSingleComment makeFetchRepliesAction formatter commentMap comment =
    let
        styles =
            [ marginBottom (px 15)
            ]

        authorName =
            S.span [ css [ marginRight (px 10) ] ]
                [ strongText comment.anonymousAuthorName
                ]
        
        replyInfo = Async ( comment.replyIds, makeFetchRepliesAction comment.id )
    in
    S.div [ ]
        [ authorName
        , secondaryText <| formatter comment.createdAt
        , S.div [ css styles ] [ primaryText comment.body ]
        , viewComments makeFetchRepliesAction formatter replyInfo commentMap
        ]



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
viewComments : FetchReplies msg -> TimeFormatter -> CommentPointers msg -> CommentMap -> StyledHtml msg
viewComments makeFetchRepliesAction formatter pointers commentMap =
    let
        viewComments_ : List Cuid -> StyledHtml msg
        viewComments_ pointerList =
            if List.length pointerList == 0 then
                S.div [] []
            else
                let
                    comments =
                        Dict.values commentMap
                        |> List.filter (\comment -> List.member comment.id pointerList)

                    viewSingleComment_ =
                        viewSingleComment makeFetchRepliesAction formatter commentMap
                in
                S.div
                    [ css [ marginLeft (px 15) ] ]
                    (List.map viewSingleComment_ comments)
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





viewCommentsSection : FetchReplies msg -> TimeFormatter -> CommentTree -> StyledHtml msg
viewCommentsSection makeFetchRepliesAction formatter { topLevelComments, comments }=
    S.div
        [ css [ marginLeft (px -15) ] ]
        [ viewComments makeFetchRepliesAction formatter (Simple topLevelComments) comments ]

