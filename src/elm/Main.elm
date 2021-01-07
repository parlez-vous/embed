module Main exposing (main)

import Ant.Button as Btn exposing (button)
import Ant.Form.View as FV
import Ant.Modal as Modal
import Api 
import Browser
import Css exposing (..)
import Data exposing (User(..))
import Data.Comment exposing (updateComment)
import Data.Cuid exposing (Cuid)
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData, mapSimpleWebData)
import ErrorReporting exposing (ReporterClient)
import Html exposing (Html)
import Html.Styled as Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes as Attr exposing (css)
import Http
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

                model = NotReady apiClient flags.gitRef flags.anonymousUsername

                getCurrentTimeCmd awaitingSessionServerResponse =
                    Task.perform ( NewCurrentTime awaitingSessionServerResponse ) Time.now
            in
            case flags.sessionToken of
                Nothing ->
                    ( model 
                    , getCurrentTimeCmd False
                    )

                Just sessionToken ->
                    let
                        -- If there's a session token,
                        -- we want to retrieve the user from the server
                        getUserCmd =
                            Task.attempt
                                (ReceivedSessionResponse flags.anonymousUsername)
                                (apiClient.getUserFromSessionToken sessionToken)
                    in
                    ( model
                    , Cmd.batch
                        [ getCurrentTimeCmd True
                        , getUserCmd
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
    Time.every fiveMinutes (NewCurrentTime False)



-- VIEW

intoSubmitCommentAction : Cuid -> Maybe Cuid -> String -> SimpleWebData User -> Maybe Msg
intoSubmitCommentAction postId parentCommentId textAreaValue webDatauser =
    case webDatauser of 
        SimpleWebData.Success user ->
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
                            , submitReply = \commentId replyTextAreaValue ->
                                intoSubmitCommentAction
                                    commentTree.postId
                                    (Just commentId)
                                    replyTextAreaValue
                                    model.user
                            }

                        commentsSection =
                            viewCommentsSection actions timeStampFormatter commentTree

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
                SimpleWebData.Success user ->
                    case user of 
                        Anonymous maybeAnonymousUsername ->
                            viewAuthenticationForm model.modal maybeAnonymousUsername

                        Authenticated _ ->
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

                NotReady _ _ _ ->
                    Styled.div [] []

                Ready appData ->
                    viewApp appData
    in
    appShell contents

