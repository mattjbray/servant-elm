module GetOneSource exposing (..)

import Http
import Json.Decode exposing (..)


getOne : Http.Request (Http.Response (Int))
getOne =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "one"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString int response.body))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
