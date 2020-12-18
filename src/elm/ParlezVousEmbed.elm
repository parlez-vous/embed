module ParlezVousEmbed exposing (init, viewApp, Model, Msg, setCurrentTime, update)

import Ant.Button as Btn exposing (button)
import Api exposing (Api)
import Browser.Navigation as Nav
import Css exposing (..)
import Data.Comment as Comment exposing (Comment, CommentTree, updateComment)
import Data.Cuid exposing (Cuid)
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData, mapSimpleWebData)
import Dict
import ErrorReporting exposing (ReporterClient)
import Html.Styled as Styled exposing (Html, fromUnstyled)
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
    , anonymousUsername : Maybe String
    , reporter : ReporterClient Msg
    }


type alias ApiRequestOutcome a = Result Http.Error a

type Msg
    = TextAreaValueChanged String
    | SubmitComment (Maybe String) Cuid (Maybe Cuid) String
    | LoadRepliesForCommentRequested Cuid
    | GoToParlezVous
    -- comments have internal state
    -- (currently text area visibility and text area value)
    -- this msg represents changes in both of these values
    | CommentChanged Comment

    -- Api outcomes
    | CommentSubmitted (ApiRequestOutcome (Time.Posix, Comment))
    | InitialPostCommentsFetched (ApiRequestOutcome CommentTree)
    | RepliesForCommentFetched Cuid (ApiRequestOutcome CommentTree)
    | ErrorReportSubmitted (ApiRequestOutcome ())



init : Maybe String -> Maybe String -> Api -> Time.Posix -> ( Model, Cmd Msg )
init gitRef maybeUsername api time =
    let
        apiClient = Api.getApiClient api

        initialModel =
            { textAreaValue = ""
            , commentTree = SimpleWebData.Loading
            , currentTime = time
            , apiClient = apiClient
            , reporter = ErrorReporting.reporterFactory apiClient ErrorReportSubmitted gitRef
            , anonymousUsername = maybeUsername
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



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToParlezVous ->
            ( model
            , Nav.load "https://parlezvous.io?ref=embed"
            )

        TextAreaValueChanged newValue ->
            simpleUpdate { model | textAreaValue = newValue }

        InitialPostCommentsFetched httpRequestResult ->
            case httpRequestResult of
                Err e ->
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

    
        SubmitComment maybeAnonymousUsername postId maybeParentCommentId commentBody ->
            let
                -- Task Stuff 
                addCommentTask = 
                    model.apiClient.addComment
                        commentBody
                        postId
                        maybeParentCommentId
                        maybeAnonymousUsername

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

                Ok ( currentTime, newComment ) ->
                    let
                        -- if the comment was a top-level comment then
                        -- reset the text area
                        newTextAreaValue =
                            if Comment.isReply newComment then
                                model.textAreaValue
                            else
                                ""

                        newModel =
                            { model
                                | commentTree =
                                    mapSimpleWebData
                                        (Comment.addNewComment newComment)
                                        model.commentTree
                                , currentTime = currentTime
                                , textAreaValue = newTextAreaValue
                            }


                        reporterMsg =
                            model.reporter.reportInvalidTimeStamps
                                currentTime
                                newComment.createdAt

                    in
                    case model.anonymousUsername of
                        Just _ ->
                            -- we already have the username in both memory and cached in
                            -- localstorage
                            ( newModel, reporterMsg )
                        
                        Nothing ->
                            ( { newModel
                                | anonymousUsername = Just newComment.anonymousAuthorName 
                              }

                            , Cmd.batch
                                [ Utils.writeToLocalStorage
                                    ( "anonymousUsername"
                                    , newComment.anonymousAuthorName
                                    )
                                , reporterMsg
                                ]
                            )
                            

        CommentChanged comment ->
            simpleUpdate
                { model | commentTree = 
                    mapSimpleWebData
                        (Comment.setComment comment)
                        model.commentTree
                }


        ErrorReportSubmitted _ ->
            simpleUpdate model
            












------------------------------------------
------------------------------------------
------------------------------------------
-- View




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
                                SubmitComment model.anonymousUsername commentTree.postId (Just commentId) replyTextAreaValue
                            }

                        commentsSection =
                            viewCommentsSection actions timeStampFormatter commentTree

                        textAreaAction =
                            SubmitComment model.anonymousUsername commentTree.postId Nothing model.textAreaValue

                        textArea =
                            topLevelTextArea TextAreaValueChanged model.textAreaValue
                            |> TextArea.toHtml textAreaAction
                    in
                    Styled.div []
                        [ Styled.div [ css [ marginBottom (px 10) ] ]
                            [ textArea ]
                        , commentsSection 
                        ]

        poweredByParlezVous =
            let
                poweredByText = button "ParlezVous"
                    |> Btn.onClick GoToParlezVous
                    |> Btn.withType Btn.Text
                    |> Btn.toHtml
                    |> fromUnstyled

            in
            Styled.div
                [ css
                    [ marginTop (px 25) 
                    , textAlign center
                    ]
                ]
                [ Styled.a [ css [ cursor pointer ] ]
                    [ poweredByText ]
                ]
    in
    Styled.div []
        [ embedContents
        , poweredByParlezVous
        ]

