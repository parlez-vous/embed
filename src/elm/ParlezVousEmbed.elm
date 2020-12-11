module ParlezVousEmbed exposing (init, viewApp, Model, Msg, setCurrentTime, update)

import Ant.Input as Input exposing (input)
import Api exposing (Api)
import Api.Input exposing (Comment, CommentTree, Cuid)
import Css exposing (Style, auto, marginRight, marginLeft, maxWidth, pct, px)
import Css.Media as Media exposing (withMedia)
import Dict
import Html exposing (Html, div)
import Html.Styled as Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Http
import RemoteData
import Time
import UI.Comment exposing (viewCommentsSection)
import Utils exposing (humanReadableTimestamp)


{-
    1. Server sends us a partial tree
    2. We walk the tree and see whether the tree is "complete" or not
        i.e. are there even more comments deeply nested in the tree that requires us to do a
        subsequent round trip to the server
-}


-- There is no such thing as "not asked" for this data type
type SimpleWebData a 
    = Loading
    | Success a
    | Failure Http.Error


mapSimpleWebData : (a -> b) -> SimpleWebData a -> SimpleWebData b
mapSimpleWebData f simpleWebData =
    case simpleWebData of
        Success data -> Success (f data)
        Loading -> Loading
        Failure e -> Failure e



updateComment : (Comment -> Comment) -> Cuid -> CommentTree -> CommentTree
updateComment f commentCuid currentTree =
    let
        newComments =
            Dict.update
            commentCuid
            (Maybe.map f)
            currentTree.comments
    in
    { currentTree | comments = newComments }


type alias Model =
    { textAreaValue : String
    , commentTree : SimpleWebData CommentTree
    , currentTime : Time.Posix
    , apiClient : Api.ApiClient Msg
    }


type Msg
    = TextAreaValueChanged String
    | InitialPostCommentsFetched (Result Http.Error CommentTree)
    | RepliesForCommentFetched Cuid (Result Http.Error CommentTree)
    | LoadRepliesForCommentRequested Cuid


init : Api -> Time.Posix -> ( Model, Cmd Msg )
init api time =
    let
        apiClient = Api.getApiClient api

        initialModel =
            { textAreaValue = ""
            , commentTree = Loading
            , currentTime = time
            , apiClient = apiClient
            }
    in
    (initialModel, apiClient.getPostComments InitialPostCommentsFetched)


setCurrentTime : Time.Posix -> Model -> Model
setCurrentTime time model =
    { model | currentTime = time
    }


simpleUpdate : Model -> ( Model, Cmd Msg )
simpleUpdate m = ( m, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaValueChanged newValue ->
            simpleUpdate { model | textAreaValue = newValue }

        InitialPostCommentsFetched httpRequestResult ->
            case httpRequestResult of
                Err e ->
                    let
                        _ = Debug.log "> Errrrr: " e
                    in
                    simpleUpdate
                        { model | commentTree =
                            Failure e
                        }

                Ok initialCommentResponse ->
                    let
                        _ = Debug.log "> initial response: " initialCommentResponse
                    in
                    simpleUpdate
                        { model | commentTree =
                            Success initialCommentResponse
                        }

        RepliesForCommentFetched commentCuid httpRequestResult ->
            case httpRequestResult of
                Err e ->
                    let
                        _ = Debug.log "> Errrrr2: " e
                    in
                    simpleUpdate
                        { model | commentTree =
                            Failure e
                        }

                -- 1. update this specific comment's reply list
                -- 2. append the new comments to the comment map
                Ok subCommentTree ->
                    let
                        -- "topLevelComments" in this case represents
                        -- direct children of the parent comment in question.
                        -- all other comments are 2nd or 3rd level descendants
                        -- i.e. replies to other replies in this api response
                        directRepliesToComment = subCommentTree.topLevelComments

                        -- update the comment in question
                        -- with the list of children reply ids
                        treeStateUpdate =
                            updateComment 
                                (\comment ->
                                    { comment | replyIds = RemoteData.Success directRepliesToComment }
                                )
                                commentCuid

                        newCommentTree =
                            mapSimpleWebData
                            (\commentTree ->
                                let 
                                    -- run the above mutation
                                    treeWithUpdatedState = treeStateUpdate commentTree
                                in
                                { treeWithUpdatedState | comments =
                                    -- add new replies / comments to flattened comment map
                                    Dict.union subCommentTree.comments treeWithUpdatedState.comments
                                }
                            )
                            model.commentTree
                    in
                    simpleUpdate
                        { model | commentTree = newCommentTree 
                        }

        LoadRepliesForCommentRequested commentCuid ->
            let
                updateCommentsInCommentTree =
                    updateComment 
                        (\comment ->
                            { comment | replyIds = RemoteData.Loading }
                        )
                        commentCuid

                newCommentTree = mapSimpleWebData updateCommentsInCommentTree model.commentTree

                tagger = RepliesForCommentFetched commentCuid
            in
            -- 1. set this specific comment's replies as RemoteData.Loading
            -- 2. issue Cmd to fetch data 
            ( { model | commentTree = newCommentTree }
            , model.apiClient.getRepliesForComment commentCuid tagger
            )





------------------------------------------
------------------------------------------
------------------------------------------
-- View

type alias MediaQueries =
    { extraSmall : Style
    , small : Style
    , medium : Style
    , large : Style
    }


extraSmallMediaQueries : Style
extraSmallMediaQueries =
    withMedia [ Media.only Media.screen [ Media.maxWidth (px 375) ] ]
        [ maxWidth (pct 95)
        ]


smallMediaQueries : Style
smallMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 376), Media.maxWidth (px 640) ] ]
        [ maxWidth (pct 94)
        ]


mediumMediaQueries : Style
mediumMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 641), Media.maxWidth (px 1007) ] ]
        [ maxWidth (px 600)
        ]



largeMediaQueries : Style
largeMediaQueries =
    withMedia [ Media.only Media.screen [ Media.minWidth (px 1008) ] ]
        [ maxWidth (px 800)
        ]



mediaQueries : MediaQueries
mediaQueries =
    { extraSmall = extraSmallMediaQueries
    , small = smallMediaQueries
    , medium = mediumMediaQueries
    , large = largeMediaQueries
    }



viewApp : Model -> Html Msg
viewApp model =
    let
        textArea =
            input TextAreaValueChanged
            |> Input.withTextAreaType { rows = 5 }
            |> Input.withPlaceholder "What are your thoughts?"
            |> Input.toHtml model.textAreaValue
            |> fromUnstyled


        commentsSection =
            case model.commentTree of
                Loading ->
                    Styled.div [] [ Styled.text "loading ..." ]

                Failure _ ->
                    -- TODO: Send error log to sentry or something
                    Styled.div [] [ Styled.text "Error while fetching comments" ]

                Success commentTree ->
                    let
                        timeStampFormatter = humanReadableTimestamp model.currentTime
                    in
                    viewCommentsSection LoadRepliesForCommentRequested timeStampFormatter commentTree

        contents =
            [ textArea, commentsSection]

        styledAppShell =
            Styled.div
                [ css
                    [ maxWidth (px 800)
                    , marginRight auto
                    , marginLeft auto
                    , mediaQueries.extraSmall
                    , mediaQueries.small
                    , mediaQueries.medium
                    , mediaQueries.large
                    ]
                ]
                contents
    in
    toUnstyled styledAppShell 

