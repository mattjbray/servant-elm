module GetBooksByTitleSource exposing (..)

import Http


getBooksByTitle : String -> Http.Request (Http.Response (Book))
getBooksByTitle capture_title =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_title |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\response ->
                    Result.map
                        (\body -> { response | body = body })
                        (decodeString decodeBook response.body))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
