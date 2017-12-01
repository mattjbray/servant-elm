module PutNothingSource exposing (..)

import Http


putNothing : Http.Request (Http.Response (()))
putNothing =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "nothing"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\response ->
                    if String.isEmpty response.body then
                        Ok { response | body = () }
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
