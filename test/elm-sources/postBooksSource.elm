module PostBooksSource exposing (..)

import Http


postBooks : Book -> Http.Request (Http.Response (NoContent))
postBooks body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                ]
        , body =
            Http.jsonBody (encodeBook body)
        , expect =
            Http.expectStringResponse
                (\response ->
                    if String.isEmpty response.body then
                        Ok { response | body = NoContent }
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
