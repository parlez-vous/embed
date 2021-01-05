module Main exposing (main)

import Ant.Button as Btn exposing (button)
import Ant.Form.View as FV
import Ant.Modal as Modal
import Api exposing (Api)
import Browser
import Css exposing (..)
import Data exposing (User(..))
import Data.Comment exposing (updateComment)
import Data.SimpleWebData as SimpleWebData exposing (SimpleWebData, mapSimpleWebData)
import ErrorReporting exposing (ReporterClient)
import Html exposing (Html)
import Html.Styled as Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Attributes as Attr exposing (css)
import Http
import Model exposing (AppData, Model(..), Msg(..))
import Task
import Time
import UI.AppShell exposing (appShell)
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
    , update = Model.update
    , subscriptions = subscriptions
    }




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







-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        fiveMinutes = 1000 * 60 * 5
    in
    Time.every fiveMinutes NewCurrentTime 


-- VIEW


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
                                SubmitComment model.user commentTree.postId (Just commentId) replyTextAreaValue
                            }

                        commentsSection =
                            viewCommentsSection actions timeStampFormatter commentTree

                        textAreaAction =
                            SubmitComment model.user commentTree.postId Nothing model.textAreaValue

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
    appShell contents

