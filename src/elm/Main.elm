module Main exposing (main)

import Ant.Button as Btn exposing (button)
import Ant.Css
import Ant.Form.View as FV
import Ant.Modal as Modal
import Api exposing (Api)
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Media as Media exposing (withMedia)
import Css.ModernNormalize as NormalizeCss
import Data exposing (UserWithToken)
import Data.Comment as Comment exposing (Comment, CommentTree, updateComment)
import Data.Cuid exposing (Cuid)
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData, mapSimpleWebData)
import Dict
import ErrorReporting exposing (ReporterClient)
import Html exposing (Html)
import Html.Styled as Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes as Attr exposing (css)
import Http
import RemoteData
import Set
import Task
import Time
import UI.AuthenticationInfo as AuthenticationInfo exposing (createAuthenticationPrompt)
import UI.Comment exposing (viewCommentsSection)
import UI.TextArea as TextArea exposing (topLevelTextArea)
import Url
import Utils


type alias Flags = 
    { apiEndpoint : String
    , siteUrl : String
    , anonymousUsername : Maybe String
    , gitRef : Maybe String
    , sessionToken : Maybe String
    }


main : Program Flags Model Msg 
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias AppData =
    { textAreaValue : String
    , commentTree : SimpleWebData CommentTree
    , currentTime : Time.Posix
    , apiClient : Api.ApiClient
    , anonymousUsername : Maybe String

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


-- MSG

type alias ApiRequestOutcome a = Result Http.Error a


type Msg
    = TextAreaValueChanged String
    | SubmitComment (Maybe String) Cuid (Maybe Cuid) String
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
    | UserLoggedIn (ApiRequestOutcome UserWithToken)


-- INIT

init : Flags -> (Model, Cmd Msg)
init flags =
    case ( Url.fromString flags.apiEndpoint, Url.fromString flags.siteUrl ) of
        ( Just apiBaseUrl, Just siteUrl ) ->
            let
                api = Api.apiFactory apiBaseUrl siteUrl
            in
            ( NotReady api flags.gitRef flags.anonymousUsername
            , Task.perform NewCurrentTime Time.now
            )

        _ ->
            ( Failed <| "invalid api endpoint or site url: " ++ "(" ++ flags.apiEndpoint ++ ", " ++ flags.siteUrl ++ ")"
            , Cmd.none
            )



-- UPDATE

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
                Utils.simpleUpdate
                    { model | commentTree = 
                        mapSimpleWebData
                            (Comment.setComment comment)
                            model.commentTree
                    }


            ErrorReportSubmitted _ ->
                Utils.simpleUpdate model
                


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
            , anonymousUsername = maybeUsername
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




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        fiveMinutes = 1000 * 60 * 5
    in
    Time.every fiveMinutes NewCurrentTime 


-- VIEW


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


authenticationForm : FV.Model AuthenticationInfo.LogInValues -> Html.Html Msg
authenticationForm = 
    FV.toHtml
        { onChange = LogInFormChanged
        , action = "submit"
        , loading = "logging in..."
        , validation = FV.ValidateOnSubmit
        }
        (AuthenticationInfo.logInForm LogInRequested)


viewApp : AppData -> Styled.Html Msg
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
                        timeStampFormatter = Utils.humanReadableTimestamp model.currentTime

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

                        authenticationPrompt = createAuthenticationPrompt AuthenticationButtonClicked

                        textArea =
                            topLevelTextArea TextAreaValueChanged model.textAreaValue authenticationPrompt
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

        modal =
            Modal.modal (authenticationForm model.logInFormState)
            |> Modal.withTitle "Log In"
            |> Modal.withOnCancel ModalStateChanged 
            |> Modal.toHtml model.modalOpen
            |> fromUnstyled
    in
    Styled.div []
        [ modal
        , embedContents
        , poweredByParlezVous
        ]



view : Model -> Html Msg
view model =
    let
        contents =
            case model of
                Failed reason ->
                    Styled.text reason

                NotReady _ _ _ ->
                    Styled.div [] []

                Ready appData ->
                    viewApp appData

    in
    toUnstyled <|
        Styled.div
            [ Attr.class "parlez-container"
            , css
                [ maxWidth (px 800)
                , marginRight auto
                , marginLeft auto
                , mediaQueries.extraSmall
                , mediaQueries.small
                , mediaQueries.medium
                , mediaQueries.large
                ]
            ]
            [ fromUnstyled NormalizeCss.globalHtml
            , fromUnstyled Ant.Css.defaultStyles
            , contents
            ]

