module Main exposing (main)

import Ant.Button as Btn exposing (button)
import Ant.Form.View as FV
import Ant.Modal as Modal
import Api 
import Browser
import Css exposing (..)
import Data exposing (User(..))
import Data.Cuid exposing (Cuid)
import Data.RemoteUser as RemoteUser exposing (RemoteUser)
import Data.SimpleWebData as SimpleWebData
import Html exposing (Html)
import Html.Styled as Styled exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import Model exposing (AppData, Model(..), ModalState(..), Msg(..))
import Task
import Time
import UI.AppShell exposing (appShell)
import UI.AuthenticationInfo as AuthenticationInfo exposing (viewAuthenticationInfo)
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
    , currentTime : Int
    }


main : Program Flags Model Msg 
main =
  Browser.element
    { init = init
    , view = view
    , update = Model.update
    , subscriptions = subscriptions
    }




-- INIT

init : Flags -> (Model, Cmd Msg)
init flags =
    case ( Url.fromString flags.apiEndpoint, Url.fromString flags.siteUrl ) of
        ( Just apiBaseUrl, Just siteUrl ) ->
            let
                apiClient = Api.getApiClient apiBaseUrl siteUrl

                user =
                    case maybeApiToken of
                        Nothing ->
                            RemoteUser.UserLoaded <| Anonymous flags.anonymousUsername

                        Just _ ->
                            RemoteUser.AwaitingUserInfoAndInteractions


                ( model, baseCmd ) =
                    Model.intoReadyState
                        flags.gitRef 
                        apiClient
                        user
                        (Time.millisToPosix flags.currentTime)

                maybeApiToken =
                    flags.sessionToken
                        |> Maybe.map Data.ApiToken
            in
            case maybeApiToken of
                -- if no token, then just get comments
                Nothing ->
                    ( model 
                    , baseCmd
                    )

                -- else, then get comments + user + interactions
                Just sessionToken ->
                    let
                        -- If there's a session token,
                        -- we want to retrieve the user from the server
                        getUserCmd =
                            Task.attempt
                                (ReceivedSessionResponse sessionToken flags.anonymousUsername)
                                (apiClient.getUserFromSessionToken sessionToken)

                        getInteractionsCmd =
                            Task.attempt
                                (ReceivedInteractionsResponse flags.anonymousUsername)
                                (apiClient.getUserInteractions sessionToken)
                    in
                    ( model
                    , Cmd.batch
                        [ baseCmd
                        , getUserCmd
                        , getInteractionsCmd
                        ]
                    )

        _ ->
            ( Failed <| "invalid api endpoint or site url: " ++ "(" ++ flags.apiEndpoint ++ ", " ++ flags.siteUrl ++ ")"
            , Cmd.none
            )







-- SUBSCRIPTIONS

-- Currently assuming that the first Msg emitted occurs
-- after the time elapses, not immediately at time 0
subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        fiveMinutes = 1000 * 60 * 5
    in
    Time.every fiveMinutes NewCurrentTime



-- VIEW

intoSubmitCommentAction : Cuid -> Maybe Cuid -> String -> RemoteUser -> Maybe Msg
intoSubmitCommentAction postId parentCommentId textAreaValue remoteUser =
    case remoteUser of 
        RemoteUser.UserLoaded user ->
            Just (SubmitComment user postId parentCommentId textAreaValue)

        _ ->
            Nothing


viewAuthenticationForm : ModalState -> Maybe String -> Styled.Html Msg
viewAuthenticationForm modalState anonymousUsername =
    let
        viewModal contents title =
            Modal.modal contents
            |> Modal.withTitle title
            |> Modal.withOnCancel (\_ -> ModalStateChanged Hidden)
            |> Modal.toHtml True
            |> fromUnstyled
    in
    case modalState of
        Hidden ->
            Styled.text ""

        ShowingLogInForm logInFormState ->
            let
                formSubmitMsg = LogInRequested logInFormState anonymousUsername 

                logInForm =
                    FV.toHtml
                        { onChange = ModalStateChanged << ShowingLogInForm
                        , action = "submit"
                        , loading = "logging in..."
                        , validation = FV.ValidateOnSubmit
                        }
                        (AuthenticationInfo.logInForm formSubmitMsg)
                        logInFormState
            in
            viewModal logInForm "Log In"

        ShowingSignUpForm signUpFormState ->
            let
                formSubmitMsg = SignUpRequested signUpFormState anonymousUsername

                signUpForm =
                    FV.toHtml
                        { onChange = ModalStateChanged << ShowingSignUpForm
                        , action = "submit"
                        , loading = "signing up..."
                        , validation = FV.ValidateOnSubmit
                        }
                        (AuthenticationInfo.signUpForm formSubmitMsg)
                        signUpFormState
            in
            viewModal signUpForm "Sign Up"



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
                            , submitCommentVote = VoteButtonClicked
                            , submitReply = \commentId replyTextAreaValue ->
                                intoSubmitCommentAction
                                    commentTree.postId
                                    (Just commentId)
                                    replyTextAreaValue
                                    model.user
                            }

                        commentsSection =
                            case model.user of
                                RemoteUser.UserLoaded user ->
                                    viewCommentsSection actions timeStampFormatter commentTree user

                                _ ->
                                    Styled.text "Loading ..."

                        textAreaAction =
                            intoSubmitCommentAction
                                commentTree.postId
                                Nothing
                                model.textAreaValue
                                model.user


                        authenticationPrompt = viewAuthenticationInfo model.user AuthenticationButtonClicked

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
            case model.user of
                RemoteUser.UserLoaded user ->
                    case user of 
                        Anonymous maybeAnonymousUsername ->
                            viewAuthenticationForm model.modal maybeAnonymousUsername

                        Authenticated _ _ _ ->
                            Styled.text ""

                _ ->
                    Styled.text ""
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

                Ready appData ->
                    viewApp appData
    in
    appShell contents

