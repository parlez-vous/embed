module ParlezVousEmbed exposing (init, viewApp, Model, Msg, setCurrentTime, update)

import Api exposing (Api)
import Css exposing (..)
import Css.Media as Media exposing (withMedia)
import Data.Comment as Comment exposing (Comment, CommentTree, updateComment)
import Data.Cuid exposing (Cuid)
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData, mapSimpleWebData)
import Dict
import Html exposing (Html)
import Html.Styled as Styled exposing (toUnstyled)
import Html.Styled.Attributes exposing (css)
import Http
import RemoteData
import Set
import Task
import Time
import UI.Comment exposing (viewCommentsSection)
import UI.TextArea as TextArea exposing (topLevelTextArea)
import Utils exposing (humanReadableTimestamp)


{-
    1. Server sends us a partial tree
    2. We walk the tree and see whether the tree is "complete" or not
        i.e. are there even more comments deeply nested in the tree that requires us to do a
        subsequent round trip to the server
-}


type alias Model =
    { textAreaValue : String
    , commentTree : SimpleWebData CommentTree
    , currentTime : Time.Posix
    , apiClient : Api.ApiClient
    }


type alias ApiRequestOutcome a = Result Http.Error a

type Msg
    = TextAreaValueChanged String
    | SubmitComment Cuid (Maybe Cuid) String
    | CommentSubmitted (ApiRequestOutcome (Time.Posix, Comment))
    | InitialPostCommentsFetched (ApiRequestOutcome CommentTree)
    | RepliesForCommentFetched Cuid (ApiRequestOutcome CommentTree)
    | LoadRepliesForCommentRequested Cuid
    | CommentChanged Comment



init : Api -> Time.Posix -> ( Model, Cmd Msg )
init api time =
    let
        apiClient = Api.getApiClient api

        initialModel =
            { textAreaValue = ""
            , commentTree = SimpleWebData.Loading
            , currentTime = time
            , apiClient = apiClient
            }

        apiRequest = Task.attempt InitialPostCommentsFetched apiClient.getPostComments
    in
    (initialModel, apiRequest)


setCurrentTime : Time.Posix -> Model -> Model
setCurrentTime time model =
    { model | currentTime = time
    }


simpleUpdate : Model -> ( Model, Cmd Msg )
simpleUpdate m = ( m, Cmd.none )


addNewComment : ( Time.Posix, Comment ) -> Model -> Model
addNewComment ( currentTime, newComment ) model =
    let
        addNewCommentToCommentMap : CommentTree -> CommentTree
        addNewCommentToCommentMap tree =
            { tree
                | comments =
                    Dict.insert newComment.id newComment tree.comments
            }

        addReply : Cuid -> SimpleWebData CommentTree
        addReply parentCommentId = 
            let
                -- updates parent with the new comment's pointer
                updateParentComment =
                    updateComment
                        (\parentComment ->
                            { parentComment
                                | replyIds =
                                    Set.insert newComment.id parentComment.replyIds

                                -- if the parent was a leaf, it not longer will be
                                , isLeaf = False
                            }
                        )
                        parentCommentId

            in
            model.commentTree
            |> mapSimpleWebData updateParentComment
            |> mapSimpleWebData addNewCommentToCommentMap


        -- adding an empty tuple argument to enforce lazyness
        addTopLevelComment : () -> SimpleWebData CommentTree
        addTopLevelComment _ =
            let
                addNewCommentPointerToTopLevelComments commentTree =
                    { commentTree
                        | topLevelComments =
                            Set.insert newComment.id commentTree.topLevelComments
                    }

            in
            model.commentTree
            |> mapSimpleWebData addNewCommentPointerToTopLevelComments
            |> mapSimpleWebData addNewCommentToCommentMap

        newCommentTreeState =
            case newComment.parentCommentId of
                Just parentCommentId ->
                    
                    addReply parentCommentId

                Nothing ->
                    addTopLevelComment ()
    in
    { model
        | commentTree = newCommentTreeState
        , currentTime = currentTime
    }




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
                            SimpleWebData.Failure e
                        }

                Ok initialCommentResponse ->
                    simpleUpdate
                        { model | commentTree =
                            SimpleWebData.Success initialCommentResponse
                        }

        RepliesForCommentFetched commentCuid httpRequestResult ->
            case httpRequestResult of
                Err e ->
                    let
                        _ = Debug.log "> Errrrr2: " e
                    in
                    simpleUpdate
                        { model | commentTree =
                            SimpleWebData.Failure e
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
                                    { comment
                                        | replyIds = Set.union comment.replyIds directRepliesToComment
                                        , remoteReplyBuffer = RemoteData.Success ()
                                    }
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
                            { comment
                                | remoteReplyBuffer = RemoteData.Loading
                            }
                        )
                        commentCuid

                newCommentTree = 
                    mapSimpleWebData updateCommentsInCommentTree model.commentTree

                tagger = RepliesForCommentFetched commentCuid

                apiRequest = Task.attempt tagger (model.apiClient.getRepliesForComment commentCuid)
            in
            -- 1. set this specific comment's replies as RemoteData.Loading
            -- 2. issue Cmd to fetch data 
            ( { model | commentTree = newCommentTree }
            , apiRequest
            )

    
        SubmitComment postId maybeParentCommentId commentBody ->
            let
                -- Task Stuff 
                addCommentTask = 
                    model.apiClient.addComment commentBody postId maybeParentCommentId

                wrapCommentInTimestamp comment =
                    Time.now
                    |> Task.map (\timestamp -> (timestamp, comment))

                tasks =
                    addCommentTask
                    |> Task.andThen wrapCommentInTimestamp
                    |> Task.attempt CommentSubmitted
            in
            -- update the time, then send the request
            ( model, tasks )

        CommentSubmitted result ->
            case result of
                Err e ->
                    simpleUpdate model

                Ok newCommentData ->
                    addNewComment newCommentData model
                    |> simpleUpdate

        CommentChanged comment ->
            let
                -- TODO: add a `setComment` function since you're replacing the comment entirely here
                commentUpdate =
                    updateComment (always comment) comment.id
            in
            simpleUpdate
                { model | commentTree = 
                    mapSimpleWebData commentUpdate model.commentTree
                }
            














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
        embedContents =
            case model.commentTree of
                SimpleWebData.Loading ->
                    Styled.div [] [ Styled.text "loading ..." ]

                SimpleWebData.Failure _ ->
                    Styled.div [] [ Styled.text "Error while fetching comments" ]

                SimpleWebData.Success commentTree ->
                    let
                        timeStampFormatter = humanReadableTimestamp model.currentTime

                        actions =
                            { loadRepliesForComment = LoadRepliesForCommentRequested
                            , updateComment = CommentChanged
                            , submitReply = \commentId replyTextAreaValue ->
                                SubmitComment commentTree.postId (Just commentId) replyTextAreaValue
                            }

                        commentsSection =
                            viewCommentsSection actions timeStampFormatter commentTree

                        textAreaAction =
                            SubmitComment commentTree.postId Nothing model.textAreaValue

                        textArea =
                            topLevelTextArea TextAreaValueChanged model.textAreaValue
                            |> TextArea.toHtml textAreaAction
                    in
                    Styled.div []
                        [ Styled.div [ css [ marginBottom (px 10) ] ]
                            [ textArea ]
                        , commentsSection 
                        ]

        embed =
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
                [ embedContents
                ]
    in
    toUnstyled embed 

