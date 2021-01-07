module Model exposing
    ( AppData
    , Model(..)
    , Msg(..)
    , ModalState(..)
    , update
    )


import Ant.Form.View as FV
import Ant.Modal as Modal
import Api exposing (ApiClient)
import Browser.Navigation as Nav
import Data exposing (User(..), UserInfo, UserInfoWithToken)
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
import UI.AuthenticationInfo as AuthenticationInfo
import Utils


type alias LogInFormState = FV.Model AuthenticationInfo.LogInValues
type alias SignUpFormState = FV.Model AuthenticationInfo.SignUpValues


type ModalState
    = Hidden
    | ShowingLogInForm LogInFormState
    | ShowingSignUpForm SignUpFormState



type alias AppData =
    { textAreaValue : String
    , commentTree : SimpleWebData CommentTree
    , currentTime : Time.Posix
    , apiClient : Api.ApiClient
    , user : SimpleWebData User

    , modal : ModalState
    
    -- for telemetry
    , reporter : ReporterClient Msg
    }


type Model
    = Ready AppData
    | NotReady ApiClient (Maybe String) (Maybe String)
    | Failed String



type alias ApiRequestOutcome a = Result Http.Error a


type alias AwaitingSessionServerResponse = Bool

type Msg
    = TextAreaValueChanged String
    | SubmitComment User Cuid (Maybe Cuid) String
    | LoadRepliesForCommentRequested Cuid
    | GoToParlezVous
    | NewCurrentTime AwaitingSessionServerResponse Time.Posix

    -- Authentication Stuff
    | AuthenticationButtonClicked AuthenticationInfo.AuthenticationRequest


    -- | ModalClosed Modal.ModalState
    -- | LogInFormChanged (FV.Model AuthenticationInfo.LogInValues)
    -- | SignUpFormChanged (FV.Model AuthenticationInfo.SignUpValues)
    | ModalStateChanged ModalState

    | LogInRequested LogInFormState (Maybe String) String String
    | SignUpRequested SignUpFormState (Maybe String) String String String


    -- comments have internal state
    -- (currently text area visibility and text area value)
    -- this msg represents changes in both of these values
    | CommentChanged Comment


    -- Api outcomes
    | CommentSubmitted User (ApiRequestOutcome (Time.Posix, Comment))
    | InitialPostCommentsFetched (ApiRequestOutcome CommentTree)
    | RepliesForCommentFetched Cuid (ApiRequestOutcome CommentTree)
    | ErrorReportSubmitted (ApiRequestOutcome ())
    | UserLoggedIn (Maybe String) (ApiRequestOutcome UserInfoWithToken)
    | ReceivedSessionResponse (Maybe String) (ApiRequestOutcome UserInfo)


-- handler functions exported for testing

-- handleCommentSubmitted : ( Time.Posix, Comment ) ->


{-| This function gets called once we receive the FIRST NewCurrentTime msg.

This function should never be called again.
-} 
intoReadyState 
    : Maybe String
   -> Maybe String
   -> ApiClient
   -> AwaitingSessionServerResponse
   -> Time.Posix
   -> ( Model, Cmd Msg )
intoReadyState gitRef anonAuthorName apiClient awaitingSessionServerResponse time =
    let
        -- On the first invocation to NewCurrent time (on `init`)
        -- we OPTIONALLY send an API request to the server to get a user
        -- IFF there is a session token in the user's browser.
        -- Otherwise, we don't even attempt to authenticate and default to
        -- the anonymous username that may or may not be stored in localstorage
        user =
            if awaitingSessionServerResponse then
                SimpleWebData.Loading
            else
                SimpleWebData.Success <| Anonymous anonAuthorName


        initialAppData =
            { textAreaValue = ""
            , modal = Hidden
            , commentTree = SimpleWebData.Loading
            , currentTime = time
            , apiClient = apiClient
            , reporter = ErrorReporting.reporterFactory apiClient ErrorReportSubmitted gitRef
            , user = user
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

        ( NotReady apiClient gitRef maybeAnonymousUsername, NewCurrentTime sessionInfo time) ->
            intoReadyState gitRef maybeAnonymousUsername apiClient sessionInfo time 

        ( NotReady apiClient gitRef maybeAnonymousUsername, _ ) ->
            ( NotReady apiClient gitRef maybeAnonymousUsername, Cmd.none )

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
            NewCurrentTime sessionInfo time ->
                Utils.simpleUpdate 
                    { model | currentTime = time }

            GoToParlezVous ->
                ( model
                , Nav.load "https://parlezvous.io?ref=embed"
                )

            ModalStateChanged newState ->
                Utils.simpleUpdate { model | modal = newState }

            -- maybeAnonymousUsername is the current value of the user's
            -- anonymousUsername that we use in UserLoggedIn as a fall back when log in fails
            LogInRequested logInFormState maybeAnonymousUsername usernameOrEmail password ->
                let
                    newModel =
                        { model 
                            | modal = ShowingLogInForm { logInFormState | state = FV.Loading }
                            , user = SimpleWebData.Loading
                        }

                    logInData =
                        { usernameOrEmail = usernameOrEmail
                        , password = password
                        }

                    logInCmd =
                        Task.attempt
                            (UserLoggedIn maybeAnonymousUsername)
                            (model.apiClient.userLogIn logInData)
                in
                ( newModel, logInCmd )

            SignUpRequested signUpFormState maybeAnonymousUsername username email password ->
                Utils.simpleUpdate model

            TextAreaValueChanged newValue ->
                Utils.simpleUpdate { model | textAreaValue = newValue }

            AuthenticationButtonClicked request ->
                let
                    emptyPasswordField =
                        { value = ""
                        , textVisible = False
                        }

                    logInFormState =
                        FV.idle
                            { usernameOrEmail = ""
                            , password = emptyPasswordField
                            }
                    
                    signUpFormState =
                        FV.idle
                            { username = ""
                            , email = ""
                            , password = emptyPasswordField
                            , passwordConfirm = emptyPasswordField
                            }

                    modalState =
                        case request of
                            AuthenticationInfo.LogIn ->
                                ShowingLogInForm logInFormState

                            AuthenticationInfo.SignUp ->
                                ShowingSignUpForm signUpFormState
                in
                Utils.simpleUpdate
                    { model | modal = modalState
                    }

            UserLoggedIn fallbackAnonUsername httpRequestResult ->
                let
                    updateModel user =
                        { model
                            | modal = Hidden
                            , user = SimpleWebData.Success user
                            
                        }
                in
                case httpRequestResult of
                    Err e ->
                        let
                            _ = Debug.todo "report this" e
                            _ = Debug.todo "UserLoggedIn Error: " e
                        in
                        Utils.simpleUpdate
                            (updateModel <| Anonymous fallbackAnonUsername)


                    Ok (user, userSessionToken) ->
                        let
                            _ = Debug.log "we did it!" ""
                            cmd = Utils.writeToLocalStorage
                                ( "sessionToken"
                                , Data.tokenToString userSessionToken
                                )
                        in
                        ( updateModel <| Authenticated user
                        , cmd
                        )


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

            ReceivedSessionResponse cachedMaybeAnonUsername httpRequestResult ->
                case httpRequestResult of
                    Err httpError ->
                        let
                            newModel =
                                { model
                                    | user = SimpleWebData.Success <| Anonymous cachedMaybeAnonUsername
                                }

                            cmd =
                                case httpError of 
                                    Http.BadStatus statusInt ->
                                        -- token expired
                                        if statusInt == 401 then
                                            Utils.removeSessionToken ()
                                        else
                                            -- TODO: report unexpected HTTP status codes here
                                            Cmd.none

                                    _ ->
                                        -- TODO: handle other HTTP errors
                                        -- https://package.elm-lang.org/packages/elm/http/latest/Http#Error
                                        Cmd.none

                        in
                        ( { model
                            | user = SimpleWebData.Success <| Anonymous cachedMaybeAnonUsername
                          }
                        , cmd
                        )

                    Ok user ->
                        Utils.simpleUpdate
                            { model
                                | user = SimpleWebData.Success <| Authenticated user
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

        
            SubmitComment user postId maybeParentCommentId commentBody ->
                let
                    -- Task Stuff 
                    addCommentTask = 
                        model.apiClient.addComment
                            commentBody
                            postId
                            maybeParentCommentId
                            user 

                    wrapCommentInTimestamp comment =
                        Time.now
                        |> Task.map (\timestamp -> (timestamp, comment))

                    tasks =
                        addCommentTask
                        |> Task.andThen wrapCommentInTimestamp
                        |> Task.attempt (CommentSubmitted user)
                in
                -- update the time, then send the request
                ( model, tasks )


            CommentSubmitted user result ->
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
                        case user of
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
                                            | user = 
                                                Just newComment.fallbackAnonUsername
                                                |> Anonymous
                                                |> SimpleWebData.Success
                                          }

                                        , Cmd.batch
                                            [ Utils.writeToLocalStorage
                                                ( "anonymousUsername"
                                                , newComment.fallbackAnonUsername
                                                )
                                            , reporterMsg
                                            ]
                                        )
                                    
                            Authenticated userInfo ->
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

