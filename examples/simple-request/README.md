# Simple Request

Assuming elm >= 0.19 && < 0.20 is installed, run:

```
elm make src/Main.elm
```

Open the generated `index.html`.

## Creating an HttpBuilder request

```
  "https://elm-lang.org/assets/public-opinion.txt"
      |> HttpBuilder.get
      |> HttpBuilder.withExpect (Http.expectString GotText)
      |> HttpBuilder.request
```

Upon completion either an `Ok` or an `Error` result will be processed

```
type Msg
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

```
