module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Generated.Pages as Pages
import Generated.Route as Route exposing (Route)
import Global
import Html
import Time
import Timeline exposing (Timeline)
import Url exposing (Url)


main : Program Flags (Timeline Model) (Timeline.Msg Msg)
main =
    Browser.application
        { init = \flags url key -> init flags url key |> Timeline.init 3000 (Time.millisToPosix 0)
        , view = Timeline.viewDocument view
        , update = Timeline.update update
        , subscriptions = Timeline.subscriptions subscriptions
        , onUrlRequest = Timeline.msg << LinkClicked
        , onUrlChange = Timeline.msg << UrlChanged
        }



-- INIT


type alias Flags =
    ()


type alias Model =
    { key : Key
    , url : Url
    , global : Global.Model
    , page : Pages.Model
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( global, globalCmd ) =
            Global.init flags url key

        ( page, pageCmd, pageGlobalCmd ) =
            Pages.init (fromUrl url) global
    in
    ( Model key url global page
    , Cmd.batch
        [ Cmd.map Global globalCmd
        , Cmd.map Global pageGlobalCmd
        , Cmd.map Page pageCmd
        ]
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Global Global.Msg
    | Page Pages.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                ( page, pageCmd, globalCmd ) =
                    Pages.init (fromUrl url) model.global
            in
            ( { model | url = url, page = page }
            , Cmd.batch
                [ Cmd.map Page pageCmd
                , Cmd.map Global globalCmd
                ]
            )

        Global globalMsg ->
            let
                ( global, globalCmd ) =
                    Global.update globalMsg model.global
            in
            ( { model | global = global }
            , Cmd.map Global globalCmd
            )

        Page pageMsg ->
            let
                ( page, pageCmd, globalCmd ) =
                    Pages.update pageMsg model.page model.global
            in
            ( { model | page = page }
            , Cmd.batch
                [ Cmd.map Page pageCmd
                , Cmd.map Global globalCmd
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.global
            |> Global.subscriptions
            |> Sub.map Global
        , model.page
            |> (\page -> Pages.subscriptions page model.global)
            |> Sub.map Page
        ]



-- any view function, WITH some model(s) as input
-- should now have (Timeline model) as input


view : Timeline Model -> Browser.Document Msg
view timeline =
    let
        model =
            Timeline.value timeline

        pageModelTimeline =
            timeline |> Timeline.map .page

        globalModelTimeline =
            timeline |> Timeline.map .global

        documentMap :
            (msg1 -> msg2)
            -> Document msg1
            -> Document msg2
        documentMap fn doc =
            { title = doc.title
            , body = List.map (Html.map fn) doc.body
            }
    in
    Global.view
        { page = Pages.view model.page model.global |> documentMap Page
        , global = model.global
        , toMsg = Global
        }


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound
