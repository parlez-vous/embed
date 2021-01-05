module Model exposing
    ( AppData
    , Model(..)
    , Msg(..)
    , update
    )


import Ant.Form.View as FV
import Ant.Modal as Modal
import Api exposing (Api)
import Browser.Navigation as Nav
import Data exposing (User(..), UserInfoWithToken)
import Data.Comment as Comment exposing (Comment, CommentTree, updateComment)
import Data.Cuid exposing (Cuid)
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData, mapSimpleWebData)
import Dict
import ErrorReporting exposing (ReporterClient)
import Http
import RemoteData
import Set
import Task
import Time
import UI.AuthenticationInfo as AuthenticationInfo exposing (createAuthenticationPrompt)
import Utils



type alias AppData =
    { textAreaValue : String
    , commentTree : SimpleWebData CommentTree
    , currentTime : Time.Posix
    , apiClient : Api.ApiClient
    , user : User

    -- authentication stuff
    , modalOpen : Modal.ModalState
    , logInFormState : FV.Model AuthenticationInfo.LogInValues
    
    -- for telemetry
    , reporter : ReporterClient Msg
    }


type Model
    = Ready AppData
    | NotReady Api (Maybe String) (Maybe String)
    | Failed String



type alias ApiRequestOutcome a = Result Http.Error a


type Msg
    = TextAreaValueChanged String
    | SubmitComment User Cuid (Maybe Cuid) String
    | LoadRepliesForCommentRequested Cuid
    | GoToParlezVous
    | NewCurrentTime Time.Posix

    -- Authentication Stuff
    | ModalStateChanged Modal.ModalState
    | AuthenticationButtonClicked AuthenticationInfo.AuthenticationRequest
    | LogInFormChanged (FV.Model AuthenticationInfo.LogInValues)
    | LogInRequested String String

    -- comments have internal state
    -- (currently text area visibility and text area value)
    -- this msg represents changes in both of these values
    | CommentChanged Comment


    -- Api outcomes
    | CommentSubmitted (ApiRequestOutcome (Time.Posix, Comment))
    | InitialPostCommentsFetched (ApiRequestOutcome CommentTree)
    | RepliesForCommentFetched Cuid (ApiRequestOutcome CommentTree)
    | ErrorReportSubmitted (ApiRequestOutcome ())
    | UserLoggedIn (ApiRequestOutcome UserInfoWithToken)


-- handler functions exported for testing

-- handleCommentSubmitted : ( Time.Posix, Comment ) ->


intoReadyState  : Maybe String -> Maybe String -> Api -> Time.Posix -> ( Model, Cmd Msg )
intoReadyState gitRef maybeUsername api time =
    let
        apiClient = Api.getApiClient api

        logInFormState =
            FV.idle
                { usernameOrEmail = ""
                , password =
                    { value = ""
                    , textVisible = False
                    }
                }

        initialAppData =
            { textAreaValue = ""
            , modalOpen = False
            , logInFormState = logInFormState
            , commentTree = SimpleWebData.Loading
            , currentTime = time
            , apiClient = apiClient
            , reporter = ErrorReporting.reporterFactory apiClient ErrorReportSubmitted gitRef
            , user = Anonymous maybeUsername
            }

        apiRequest = Task.attempt InitialPostCommentsFetched apiClient.getPostComments
    in
    ( Ready initialAppData
    , apiRequest
    )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Failed reason, _ ) ->
            ( Failed reason, Cmd.none )

        ( NotReady api gitRef maybeAnonymousUsername, NewCurrentTime time ) ->
            intoReadyState gitRef maybeAnonymousUsername api time

        ( NotReady api gitRef maybeAnonymousUsername, _ ) ->
            ( NotReady api gitRef maybeAnonymousUsername, Cmd.none )

        ( Ready embedModel, _ ) ->
            updateReadyModel msg embedModel



simpleUpdate : AppData -> ( Model, Cmd Msg )
simpleUpdate appData =
    Utils.simpleUpdate appData
    |> Tuple.mapFirst Ready


updateReadyModel : Msg -> AppData -> ( Model, Cmd Msg )
updateReadyModel msg model =
    Tuple.mapFirst Ready <|
        case msg of
            NewCurrentTime time ->
                Utils.simpleUpdate 
                    { model | currentTime = time }


            GoToParlezVous ->
                ( model
                , Nav.load "https://parlezvous.io?ref=embed"
                )

            ModalStateChanged newState ->
                Utils.simpleUpdate { model | modalOpen = newState } 

            LogInFormChanged newState ->
                Utils.simpleUpdate { model | logInFormState = newState }

            LogInRequested usernameOrEmail password ->
                let
                    logInFormState = model.logInFormState

                    newModel =
                        { model | logInFormState =
                            { logInFormState | state = FV.Loading
                            }
                        }

                    _ = Debug.log "LogInRequested:" (usernameOrEmail ++ ", " ++ password)

                    logInData =
                        { usernameOrEmail = usernameOrEmail
                        , password = password
                        }

                    logInCmd = Task.attempt UserLoggedIn (model.apiClient.userLogIn logInData)
                in
                ( newModel, logInCmd )

            TextAreaValueChanged newValue ->
                Utils.simpleUpdate { model | textAreaValue = newValue }

            AuthenticationButtonClicked request ->
                Utils.simpleUpdate { model | modalOpen = True }

            UserLoggedIn httpRequestResult ->
                case httpRequestResult of
                    Err e ->
                        let
                            _ = Debug.todo "report this" e
                            _ = Debug.todo "UserLoggedIn Error: " e
                        in
                        Utils.simpleUpdate { model | modalOpen = False }

                    Ok (user, userSessionToken) ->
                        let
                            _ = Debug.log "we did it!" ""
                            cmd = Utils.writeToLocalStorage
                                ( "sessionToken"
                                , Data.tokenToString userSessionToken
                                )
                        in
                        ( { model | modalOpen = False }, cmd )


            InitialPostCommentsFetched httpRequestResult ->
                case httpRequestResult of
                    Err e ->
                        Utils.simpleUpdate
                            { model | commentTree =
                                SimpleWebData.Failure e
                            }

                    Ok initialCommentResponse ->
                        Utils.simpleUpdate
                            { model | commentTree =
                                SimpleWebData.Success initialCommentResponse
                            }

            RepliesForCommentFetched commentCuid httpRequestResult ->
                case httpRequestResult of
                    Err e ->
                        Utils.simpleUpdate
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
                        Utils.simpleUpdate
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
                        Utils.simpleUpdate model

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
                        case model.user of
                            Anonymous maybeAnonymousUsername ->
                                case maybeAnonymousUsername of
                                    Just _ ->
                                        -- we already have the username in both memory and cached in
                                        -- localstorage
                                        ( newModel, reporterMsg )
                                    
                                    -- In the event that the user is not logged in
                                    -- AND we don't yet have a anonymous username for them
                                    -- grab the server-generated username from the new comment and
                                    -- save it in:
                                    --   - the model
                                    --   - localstorage (for future sessions)
                                    Nothing ->
                                        ( { newModel
                                            | user = Anonymous <| Just newComment.anonymousAuthorName 
                                          }

                                        , Cmd.batch
                                            [ Utils.writeToLocalStorage
                                                ( "anonymousUsername"
                                                , newComment.anonymousAuthorName
                                                )
                                            , reporterMsg
                                            ]
                                        )
                                    
                            Authenticated user ->
                                ( newModel, reporterMsg )

            CommentChanged comment ->
                Utils.simpleUpdate
                    { model | commentTree = 
                        mapSimpleWebData
                            (Comment.setComment comment)
                            model.commentTree
                    }


            ErrorReportSubmitted _ ->
                Utils.simpleUpdate model

