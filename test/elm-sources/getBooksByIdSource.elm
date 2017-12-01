module GetBooksByIdSource exposing (..)

import Http


getBooksById : Int -> Http.Request (Http.Response (Book))
getBooksById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> toString |> Http.encodeUri
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
