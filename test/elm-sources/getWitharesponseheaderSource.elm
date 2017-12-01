module GetWithAResponseHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWitharesponseheader : Http.Request (Http.Response (String))
getWitharesponseheader =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-a-response-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString string response.body))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
