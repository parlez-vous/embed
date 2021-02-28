module Model exposing
    ( AppData
    , Model(..)
    , Msg(..)
    , ModalState(..)
    , intoReadyState
    , update
    )


import Ant.Form.View as FV
import Api exposing (ApiClient, ApiRequestOutcome)
import Browser.Navigation as Nav
import Data exposing (ApiToken, User(..), UserInfo, UserInfoWithToken, VoteType(..), VoteAction(..))
import Data.Comment as Comment exposing (Comment, CommentTree, updateComment)
import Data.Cuid exposing (Cuid)
import Data.RemoteUser as RemoteUser exposing (RemoteUser)
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
import Data exposing (Interactions)


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
    , user : RemoteUser

    , modal : ModalState
    
    -- for telemetry
    , reporter : ReporterClient Msg
    }


type Model
    = Ready AppData
    | Failed String





type Msg
    = TextAreaValueChanged String
    | SubmitComment User Cuid (Maybe Cuid) String
    | LoadRepliesForCommentRequested Cuid
    | GoToParlezVous
    | NewCurrentTime Time.Posix
    | VoteButtonClicked Cuid VoteType

    -- Authentication Stuff
    | AuthenticationButtonClicked AuthenticationInfo.AuthenticationRequest
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
    | ReceivedSessionResponse ApiToken (Maybe String) (ApiRequestOutcome UserInfo)
    | ReceivedInteractionsResponse (Maybe String) (ApiRequestOutcome Interactions)
    | ReceivedVoteResponse (ApiRequestOutcome ())


-- handler functions exported for testing

-- handleCommentSubmitted : ( Time.Posix, Comment ) ->


{-| This function gets called once we receive the FIRST NewCurrentTime msg.

This function should never be called again.
-} 
intoReadyState 
    : Maybe String
   -> ApiClient
   -> RemoteUser
   -> Time.Posix
   -> ( Model, Cmd Msg )
intoReadyState gitRef apiClient user time =
    let
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

        ( Ready embedModel, _ ) ->
            updateReadyModel msg embedModel



setFormSubmittingState : (FV.Model a -> ModalState) -> FV.Model a -> AppData -> AppData
setFormSubmittingState intoModalState formState appData =
    { appData
        | modal =
            intoModalState { formState | state = FV.Loading }
        , user = RemoteUser.AwaitingUserInfoAndInteractions
    }


updateReadyModel : Msg -> AppData -> ( Model, Cmd Msg )
updateReadyModel msg model =
    Tuple.mapFirst Ready <|
        case msg of
            NewCurrentTime time ->
                Utils.simpleUpdate 
                    { model | currentTime = time }

            GoToParlezVous ->
                ( model
                , Utils.openInNewTab "https://parlezvous.io?ref=embed"
                )

            VoteButtonClicked commentId voteType ->
                let
                    _ = Debug.log "VoteButtonClicked: " (commentId, voteType) 

                    outgoingVote interactions clickedVote =
                        let
                            setUp = ( SetUp, 1 )
                            setDown = ( SetDown, -1 )
                            setNeutral = ( SetNeutral, 0 )

                            defaultVoteAction =
                                case voteType of
                                    Up -> setUp
                                    Down -> setDown

                        in
                        Dict.get commentId interactions.commentVotes
                        |> Maybe.map
                            (\{ value } ->
                                if value > 0 then
                                    case clickedVote of
                                        Up -> setNeutral -- cancel upvote
                                        Down -> setDown
                                else if value < 0 then
                                    case clickedVote of
                                        Up -> setUp
                                        Down -> setNeutral -- cancel downvote
                                else
                                    defaultVoteAction
                            )
                        |> Maybe.withDefault defaultVoteAction


                in
                case model.user of
                    RemoteUser.UserLoaded user ->
                        case user of
                            Anonymous _ ->
                                -- TODO: trigger modal popup here
                                Utils.simpleUpdate model
                                
                            Authenticated userInfo interactions apiToken ->
                                let
                                    ( voteAction, voteValue ) =
                                        outgoingVote interactions voteType

                                    newInteractions =
                                        let
                                            newCommentVotes =
                                                Dict.insert
                                                    commentId
                                                    { commentId = commentId, value = voteValue }
                                                    interactions.commentVotes
                                        in
                                        { interactions | commentVotes = newCommentVotes
                                        }


                                    task =
                                        model.apiClient.submitVoteForComment
                                            apiToken
                                            commentId
                                            voteAction
                                in
                                ( { model | user =
                                        RemoteUser.UserLoaded 
                                            (Authenticated userInfo newInteractions apiToken)
                                  }
                                , Task.attempt
                                    ReceivedVoteResponse
                                    task
                                )

                    _ ->
                        Utils.simpleUpdate model

            ModalStateChanged newState ->
                Utils.simpleUpdate { model | modal = newState }

            -- maybeAnonymousUsername is the current value of the user's
            -- anonymousUsername that we use in UserLoggedIn as a fall back when log in fails
            LogInRequested logInFormState maybeAnonymousUsername usernameOrEmail password ->
                let
                    logInData =
                        { usernameOrEmail = usernameOrEmail
                        , password = password
                        }

                    logInCmd =
                        Task.attempt
                            (UserLoggedIn maybeAnonymousUsername)
                            (model.apiClient.userLogIn logInData)
                in
                ( setFormSubmittingState ShowingLogInForm logInFormState model 
                , logInCmd
                )

            SignUpRequested signUpFormState maybeAnonymousUsername username email password ->
                let
                    signUpData =
                        { username = username
                        , email = email
                        , password = password
                        }

                    signUpCmd =
                        Task.attempt
                            (UserLoggedIn maybeAnonymousUsername)
                            (model.apiClient.userSignUp signUpData)
                in
                ( setFormSubmittingState ShowingSignUpForm signUpFormState model
                , signUpCmd
                )


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
                    newModel =
                        { model
                            | modal = Hidden
                            , user =
                                RemoteUser.setUserInfo
                                    fallbackAnonUsername
                                    httpRequestResult
                                    model.user
                        }
                in
                case httpRequestResult of
                    -- TODO: report this
                    Err e ->
                        Utils.simpleUpdate newModel


                    Ok (_, (Data.ApiToken token) as apiToken) ->
                        let
                            getInteractionsCmd =
                                Task.attempt
                                    (ReceivedInteractionsResponse fallbackAnonUsername)
                                    (model.apiClient.getUserInteractions apiToken)
                        in
                        ( newModel
                        , Cmd.batch
                            [ Utils.writeToLocalStorage
                                ( "sessionToken", token
                                )
                            , getInteractionsCmd 
                            ]
                        )


            InitialPostCommentsFetched httpRequestResult ->
                case httpRequestResult of
                    Err e ->
                        Utils.simpleUpdate
                            { model | commentTree =
                                SimpleWebData.Failure (Debug.log "> " e)
                            }

                    Ok initialCommentResponse ->
                        Utils.simpleUpdate
                            { model | commentTree =
                                SimpleWebData.Success initialCommentResponse
                            }

            ReceivedVoteResponse httpRequestResult ->
                let
                    _ = Debug.log "ReceivedVoteResponse: " httpRequestResult
                in
                Utils.simpleUpdate model

            ReceivedSessionResponse apiToken cachedMaybeAnonUsername httpRequestResult ->
                let
                    resultWithToken =
                        Result.map (\user -> (user, apiToken)) httpRequestResult

                    cmd = 
                        case httpRequestResult of
                            Ok _ ->
                                Cmd.none

                            Err httpError ->
                                case httpError of
                                    Http.BadStatus statusValue ->
                                        if statusValue == 401 then
                                            Utils.removeSessionToken ()
                                        else
                                            -- TODO: report unexpected HTTP status code
                                            Cmd.none

                                    _ -> 
                                        -- TODO: handle other HTTP errors
                                        -- https://package.elm-lang.org/packages/elm/http/latest/Http#Error
                                        Cmd.none
                in
                ( { model
                    | user = 
                        Debug.log "ReceivedSessionResponse"
                        (RemoteUser.setUserInfo
                            cachedMaybeAnonUsername
                            resultWithToken
                            model.user)
                  }
                , cmd
                )

            ReceivedInteractionsResponse fallbackAnonUsername interactionsHttpResult ->
                Utils.simpleUpdate
                    { model | user =
                        Debug.log "ReceivedInteractionsResponse"
                        (RemoteUser.setInteractions
                            fallbackAnonUsername
                            interactionsHttpResult
                            model.user)
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
                                                |> RemoteUser.UserLoaded
                                          }

                                        , Cmd.batch
                                            [ Utils.writeToLocalStorage
                                                ( "anonymousUsername"
                                                , newComment.fallbackAnonUsername
                                                )
                                            , reporterMsg
                                            ]
                                        )
                                    
                            Authenticated _ _ _ ->
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

