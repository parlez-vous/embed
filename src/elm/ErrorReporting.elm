module ErrorReporting exposing (ReporterClient, reporterFactory)

import Api exposing (ApiClient)
import Http
import Task
import Time exposing (Posix)
import Utils


type alias HttpResponseOutcome =
    Result Http.Error ()


type alias Tagger msg =
    HttpResponseOutcome -> msg


type alias ReporterClient msg =
    { reportInvalidTimeStamps : ReportInvalidTimeStamps msg
    }


type alias Reporter msg =
    { apiClient : ApiClient
    , toMsg : Tagger msg
    , gitRef : Maybe String
    }


reporterFactory : ApiClient -> Tagger msg -> Maybe String -> ReporterClient msg
reporterFactory apiClient tagger maybeGitRef =
    let
        reporter =
            { apiClient = apiClient
            , toMsg = tagger
            , gitRef = maybeGitRef
            }
    in
    { reportInvalidTimeStamps = reportInvalidTimeStamps reporter
    }


type alias ReportInvalidTimeStamps msg =
    Posix -> Posix -> Cmd msg


reportInvalidTimeStamps : Reporter msg -> ReportInvalidTimeStamps msg
reportInvalidTimeStamps { apiClient, toMsg, gitRef } currentTime newCommentTime =
    let
        timeDiff =
            Utils.timeDiff currentTime newCommentTime
    in
    -- don't do reporting in non-production environments, which shouldn't have a git ref
    case gitRef of
        Just ref ->
            if timeDiff < 0 then
                let
                    errorMsg =
                        "Invalid timestamps for newly added comment. "
                            ++ ("CurrentTime: " ++ (String.fromInt <| Time.posixToMillis currentTime))
                            ++ " - "
                            ++ ("New Comment TimeStamp " ++ (String.fromInt <| Time.posixToMillis newCommentTime))
                in
                Task.attempt toMsg <|
                    apiClient.reportError
                        { ref = ref
                        , message = errorMsg
                        }

            else
                Cmd.none

        Nothing ->
            Cmd.none
