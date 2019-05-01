# elm-http-builder

Pipeable functions for building HTTP requests

```elm
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder as UrlBuilder


type Status a = Loading | Loaded a | Failure


type alias Model = { items : Status (List String) }


itemsDecoder : Decode.Decoder (List String)
itemsDecoder =
    Decode.list Decode.string


itemEncoder : String -> Encode.Value
itemEncoder item =
    Encode.object
        [ ("item", Encode.string item) ]


{-| addItem will send a post request to
`"http://example.com/api/items?hello=world"` with the given JSON body and a
custom header. It'll try to decode with `itemsDecoder`.

-}
addItem : String -> Cmd Msg
addItem item =
    UrlBuilder.crossOrigin
        "http://example.com"
        [ "api", "items" ]
        [ UrlBuilder.string "hello" "world" ]
        |> HttpBuilder.post
        |> HttpBuilder.withHeader "X-My-Header" "Some Header Value"
        |> HttpBuilder.withJsonBody (itemEncoder item)
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson GotItem itemsDecoder)
        |> HttpBuilder.request


type Msg = GotItem (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotItem (Ok items) ->
          ( { model | items = Loaded items }
          , Cmd.none
          )

        GotItem (Err err) ->
          ( { model | items = Failure } , Cmd.none)

```
