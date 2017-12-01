module GetOneWithDynamicUrlSource exposing (..)

import Http
import Json.Decode exposing (..)


getOne : String -> Http.Request (Http.Response (Int))
getOne urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
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
