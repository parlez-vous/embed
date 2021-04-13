module UI.Comment exposing (viewCommentsSection)

{-| UI modules for rendering a tree of comments.
-}

import Ant.Button as Btn exposing (Button, button)
import Ant.Icons as Icon exposing (Icon, downOutlined, upOutlined)
import Ant.Typography.Text as Text exposing (Text, text)
import Css exposing (..)
import Data exposing (Interactions, User(..), VoteType(..))
import Data.Comment as Comment exposing (Comment, CommentMap, CommentTree)
import Data.Cuid exposing (Cuid)
import Dict
import Html.Styled as S exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import RemoteData exposing (WebData)
import Set exposing (Set)
import Time
import UI.TextArea as TextArea
import Utils


type alias StyledHtml a =
    S.Html a


type alias TimeFormatter =
    Time.Posix -> String


type alias Effects msg =
    { loadRepliesForComment : Cuid -> msg
    , updateComment : Comment -> msg
    , submitReply : Cuid -> String -> Maybe msg
    , submitCommentVote : Cuid -> VoteType -> msg
    }


type CommentPointers msg
    = Simple (Set Cuid)
    | Async
        -- loaded ids
        ( Set Cuid
          -- buffer state
        , WebData ()
          -- "load more" action
        , msg
        )


renderText : Text -> StyledHtml msg
renderText =
    Text.toHtml >> fromUnstyled


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



-- Color of vote icons depends on the interaction that the user
-- has had on a particular comment
-- Either they have a (+1) upvote or a (-1) downvote


renderVoteIcon : Effects msg -> Comment -> Maybe Interactions -> VoteType -> Icon msg -> StyledHtml msg
renderVoteIcon { submitCommentVote } comment maybeInteractions vote icon =
    let
        defaultColor =
            ( "color", "#737373" )

        chosenVoteColor =
            ( "color", "#4ba9ff" )

        voteIconColor =
            case maybeInteractions of
                Nothing ->
                    defaultColor

                Just interactions ->
                    -- this represents the logged-in users
                    -- vote for this comment
                    interactions.commentVotes
                        |> Dict.get comment.id
                        |> Maybe.map
                            (\{ value } ->
                                if value == 1 && vote == Up then
                                    chosenVoteColor

                                else if value == -1 && vote == Down then
                                    chosenVoteColor

                                else
                                    defaultColor
                            )
                        |> Maybe.withDefault defaultColor

        iconHtml =
            icon
                |> Icon.withStyles [ voteIconColor ]
                |> Icon.toHtml
                |> fromUnstyled
    in
    S.span
        [ css [ cursor pointer ]
        , onClick (submitCommentVote comment.id vote)
        ]
        [ iconHtml ]


commentActionButton : String -> msg -> StyledHtml msg
commentActionButton value msg =
    S.button
        [ css
            [ border zero
            , paddingLeft zero
            , fontSize (px 11)
            , cursor pointer
            , backgroundColor (hex "#fff")
            , marginBottom (px 15)
            ]
        , onClick msg
        ]
        [ S.text value ]


replyTextarea : Comment -> Effects msg -> StyledHtml msg
replyTextarea comment effects =
    let
        ( textAreaVisible, textAreaValue ) =
            comment.textAreaState

        updateTextArea =
            \val ->
                effects.updateComment
                    { comment
                        | textAreaState =
                            Tuple.mapSecond (always val) comment.textAreaState
                    }

        textAreaAction =
            effects.submitReply comment.id textAreaValue

        textArea =
            TextArea.replyTextArea comment updateTextArea
                |> TextArea.toHtml textAreaAction
    in
    if textAreaVisible then
        textArea

    else
        S.text ""


{-|

    @arg depth
        current depth of tree: From 0 -> n

    @arg effects
        The various kinds of effects that can be emitted / produced

    @arg formatter
        a function that formats Posix timestamps into human readable "distance" strings.
        i.e. "5 minutes ago"

    @arg pointers
        cuid pointers that represent the current level of comments to be rendered in a recursive
        tree of comments. Can be Simple, i.e. they are immediately loaded, or Async, meaning we have to do
        a subsequent round-trip to the back end to get this information.

    @arg commentMap
        a flattened hashmap that represents a recursive tree of comments (i.e. just like Reddit)

-}
viewComments :
    Int
    -> Effects msg
    -> TimeFormatter
    -> CommentPointers msg
    -> CommentMap
    -> Maybe Interactions
    -> StyledHtml msg
viewComments depth effects formatter pointers commentMap maybeInteractions =
    let
        viewComments_ : Set Cuid -> StyledHtml msg
        viewComments_ pointerSet =
            if Set.isEmpty pointerSet then
                S.text ""

            else
                let
                    comments =
                        Comment.getCommentsFromPointers commentMap pointerSet

                    optionalBorderLeft =
                        if depth > 0 then
                            [ borderLeft3 (px 1) dotted (hex "#ededed")
                            , hover
                                [ borderLeft3 (px 1) dotted (hex "#4ba9ff")
                                ]
                            ]

                        else
                            [ border zero ]
                in
                S.div
                    [ css <| paddingLeft (px 10) :: optionalBorderLeft
                    ]
                    (List.map viewSingleComment comments)

        -- this is embedded inside of view comments to
        -- take advantage of closures
        viewSingleComment : Comment -> StyledHtml msg
        viewSingleComment comment =
            let
                authorName =
                    S.span [ css [ marginRight (px 10) ] ]
                        [ strongText <| Utils.getAuthorName comment
                        ]

                replyInfo =
                    Async
                        ( comment.replyIds
                        , comment.remoteReplyBuffer
                        , effects.loadRepliesForComment comment.id
                        )

                replyButton =
                    let
                        update =
                            effects.updateComment
                                { comment
                                    | textAreaState =
                                        Tuple.mapFirst not comment.textAreaState
                                }
                    in
                    commentActionButton "reply" update

                -- Includes the folding icon
                -- and upvote / downvote icons
                viewCommentSidebar =
                    let
                        -- ensure icon is rendered as monospaced
                        fontFamilyList =
                            [ "Consolas"
                            , "Monaco"
                            , "monospace"
                            ]

                        commentFoldingText =
                            if comment.isFolded then
                                "[+]"

                            else
                                "[-]"

                        sidebarStyles =
                            [ marginRight (px 10)
                            , fontSize (pct 63)
                            , position relative
                            , top (px 2)
                            , displayFlex
                            , flexDirection column
                            ]

                        updatedComment =
                            { comment
                                | isFolded = not comment.isFolded
                            }

                        commentFoldingIcon =
                            S.span
                                [ css
                                    [ fontFamilies fontFamilyList
                                    , hover [ cursor pointer ]
                                    ]
                                , onClick (effects.updateComment updatedComment)
                                ]
                                [ S.text commentFoldingText ]

                        votingIcons =
                            if comment.isFolded then
                                S.text ""

                            else
                                let
                                    renderVoteIcon_ =
                                        renderVoteIcon effects comment maybeInteractions
                                in
                                S.div
                                    [ css
                                        [ marginTop (px 13)
                                        , position relative
                                        , left (px 2)
                                        ]
                                    ]
                                    [ renderVoteIcon_ Up upOutlined
                                    , renderVoteIcon_ Down downOutlined
                                    ]
                    in
                    S.div [ css sidebarStyles ]
                        [ commentFoldingIcon
                        , votingIcons
                        ]

                maybeViewComment =
                    if comment.isFolded then
                        [ S.div [ css [ marginBottom (px 10) ] ] []
                        ]

                    else
                        [ S.div [ css [ marginBottom (px 15) ] ]
                            [ S.div [ css [ overflowWrap breakWord ] ] [ primaryText comment.body ]
                            , replyButton
                            , replyTextarea comment effects
                            ]
                        , viewComments (depth + 1) effects formatter replyInfo commentMap maybeInteractions
                        ]
            in
            S.div [ css [ displayFlex ] ]
                [ viewCommentSidebar
                , S.div [ css [ width (pct 100) ] ]
                    ([ authorName
                     , secondaryText <| formatter comment.createdAt
                     ]
                        ++ maybeViewComment
                    )
                ]
    in
    case pointers of
        Simple pointerSet ->
            viewComments_ pointerSet

        Async ( pointerSet, bufferState, fetchReplies ) ->
            case bufferState of
                RemoteData.NotAsked ->
                    let
                        loadMoreBtn =
                            link "load more comments"
                                |> Btn.onClick fetchReplies
                                |> Btn.toHtml
                                |> fromUnstyled
                    in
                    S.div []
                        [ viewComments_ pointerSet
                        , loadMoreBtn
                        ]

                RemoteData.Loading ->
                    S.div []
                        [ viewComments_ pointerSet
                        , S.div [ css [ marginBottom (px 15) ] ]
                            [ secondaryText "loading ..." ]
                        ]

                RemoteData.Failure error ->
                    S.div []
                        [ viewComments_ pointerSet
                        , S.text "error loading more comments"
                        ]

                RemoteData.Success _ ->
                    viewComments_ pointerSet


viewCommentsSection :
    Effects msg
    -> TimeFormatter
    -> CommentTree
    -> User
    -> StyledHtml msg
viewCommentsSection effects formatter { topLevelComments, comments } user =
    let
        rootDepth =
            0

        maybeInteractions =
            case user of
                Authenticated _ interactions _ ->
                    Just interactions

                Anonymous _ ->
                    Nothing
    in
    S.div
        [ css [ marginLeft (px -10) ] ]
        [ viewComments
            rootDepth
            effects
            formatter
            (Simple topLevelComments)
            comments
            maybeInteractions
        ]
