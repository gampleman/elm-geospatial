module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Example01
import Example02
import Example03
import ExampleDebug
import Html
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, oneOf, s, string)


type Page
    = NotFound
    | Example01 Example01.Model
    | Example02 Example02.Model
    | Example03 Example03.Model
    | ExampleDebug ExampleDebug.Model


type alias Model =
    { page : Page, key : Navigation.Key }


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | Noop
    | Example02Msg Example02.Msg
    | Example03Msg Example03.Msg
    | ExampleDebugMsg ExampleDebug.Msg


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    initPageFromUrl { key = key, page = NotFound } url


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "", body = [] }

        Example01 sub ->
            mapDocument (always Noop) (Example01.view sub)

        Example02 sub ->
            mapDocument Example02Msg (Example02.view sub)

        Example03 sub ->
            mapDocument Example03Msg (Example03.view sub)

        ExampleDebug sub ->
            mapDocument ExampleDebugMsg (ExampleDebug.view sub)


mapDocument : (a -> b) -> Browser.Document a -> Browser.Document b
mapDocument function a =
    { title = a.title, body = List.map (Html.map function) a.body }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( ChangedUrl url, _ ) ->
            initPageFromUrl model url

        ( Example02Msg submsg, Example02 submodel ) ->
            let
                ( newSubModel, newCmd ) =
                    Example02.update submsg submodel
            in
            ( { model | page = Example02 newSubModel }, Cmd.map Example02Msg newCmd )

        ( Example03Msg submsg, Example03 submodel ) ->
            let
                ( newSubModel, newCmd ) =
                    Example03.update submsg submodel
            in
            ( { model | page = Example03 newSubModel }, Cmd.map Example03Msg newCmd )

        ( ExampleDebugMsg submsg, ExampleDebug submodel ) ->
            let
                ( newSubModel, newCmd ) =
                    ExampleDebug.update submsg submodel
            in
            ( { model | page = ExampleDebug newSubModel }, Cmd.map ExampleDebugMsg newCmd )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type UrlResult
    = Url01
    | Url02
    | Url03
    | UrlDebug


urlParser =
    oneOf
        [ Parser.map Url01 (s "example01")
        , Parser.map Url02 (s "example02")
        , Parser.map Url03 (s "example03")
        , Parser.map UrlDebug (s "debug")
        ]


initPageFromUrl : Model -> Url -> ( Model, Cmd Msg )
initPageFromUrl model url =
    case Parser.parse urlParser url of
        Just Url01 ->
            let
                ( submodel, cmd ) =
                    Example01.init ()
            in
            ( { model | page = Example01 submodel }, Cmd.none )

        Just Url02 ->
            let
                ( submodel, cmd ) =
                    Example02.init ()
            in
            ( { model | page = Example02 submodel }, Cmd.map Example02Msg cmd )

        Just Url03 ->
            let
                ( submodel, cmd ) =
                    Example03.init ()
            in
            ( { model | page = Example03 submodel }, Cmd.map Example03Msg cmd )

        Just UrlDebug ->
            let
                ( submodel, cmd ) =
                    ExampleDebug.init ()
            in
            ( { model | page = ExampleDebug submodel }, Cmd.map ExampleDebugMsg cmd )

        Nothing ->
            let
                ( submodel, cmd ) =
                    Example01.init ()
            in
            ( { model | page = Example01 submodel }, Cmd.none )


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
