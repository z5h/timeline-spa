module Page exposing
    ( Page, Document, Bundle
    , upgrade
    , static, sandbox, element, component
    )

{-|

@docs Page, Document, Bundle

@docs upgrade

@docs static, sandbox, element, component

-}

import Browser
import Global
import Html
import Spa
import Time
import Timeline exposing (Timeline)


type alias Document msg =
    Browser.Document msg


type alias Page flags model msg =
    { init : Global.Model -> flags -> ( Timeline model, Cmd msg, Cmd Global.Msg )
    , update : Global.Model -> msg -> Timeline model -> ( Timeline model, Cmd msg, Cmd Global.Msg )
    , view : Global.Model -> Timeline model -> Document msg
    , subscriptions : Global.Model -> Timeline model -> Sub msg
    }


type alias Bundle msg =
    Spa.Bundle msg


upgrade :
    (pageModel -> model)
    -> (pageMsg -> msg)
    -> Page pageFlags pageModel pageMsg
    ->
        { init : pageFlags -> Global.Model -> ( model, Cmd msg, Cmd Global.Msg )
        , update : pageMsg -> pageModel -> Global.Model -> ( model, Cmd msg, Cmd Global.Msg )
        , bundle : Timeline pageModel -> Global.Model -> Bundle msg
        }
upgrade toModel toMsg page =
    { init =
        \flags global ->
            let
                ( model, cmd, globalCmd ) =
                    page.init global flags
            in
            ( toModel (Timeline.value model)
            , Cmd.map toMsg cmd
            , globalCmd
            )
    , update =
        \msg model global ->
            let
                ( model_, cmd, globalCmd ) =
                    page.update global msg (toTimeline model)
            in
            ( toModel (Timeline.value model_)
            , Cmd.map toMsg cmd
            , globalCmd
            )
    , bundle =
        \model global ->
            { view =
                let
                    doc =
                        page.view global model
                in
                { title = doc.title
                , body = List.map (Html.map toMsg) doc.body
                }
            , subscriptions =
                Sub.map toMsg (page.subscriptions global model)
            }
    }


static : { view : Document msg } -> Page flags () msg
static page =
    { init = \_ _ -> ( toTimeline (), Cmd.none, Cmd.none )
    , update = \_ _ modelTimeline -> ( modelTimeline, Cmd.none, Cmd.none )
    , view = \_ _ -> page.view
    , subscriptions = \_ _ -> Sub.none
    }


toTimeline : model -> Timeline model
toTimeline model =
    Timeline.init 0 (Time.millisToPosix 0) ( model, Cmd.none )
        |> Tuple.first


sandbox :
    { init : model
    , update : msg -> model -> model
    , view : Timeline model -> Document msg
    }
    -> Page flags model msg
sandbox page =
    { init = \_ _ -> ( toTimeline page.init, Cmd.none, Cmd.none )
    , update = \_ msg modelTimeline -> ( page.update msg (Timeline.value modelTimeline) |> toTimeline, Cmd.none, Cmd.none )
    , view = \_ modelTimeline -> page.view modelTimeline
    , subscriptions = \_ _ -> Sub.none
    }


element :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : Timeline model -> Document msg
    }
    -> Page flags model msg
element page =
    { init =
        \_ flags ->
            let
                ( model, cmd ) =
                    page.init flags
            in
            ( toTimeline model, cmd, Cmd.none )
    , update =
        \_ msg modelTimeline ->
            let
                ( model, cmd ) =
                    page.update msg (Timeline.value modelTimeline)
            in
            ( model |> toTimeline, cmd, Cmd.none )
    , view = \_ modelTimeline -> page.view modelTimeline
    , subscriptions = \_ modelTimeline -> page.subscriptions (Timeline.value modelTimeline)
    }


component :
    { init : Global.Model -> flags -> ( model, Cmd msg, Cmd Global.Msg )
    , update : Global.Model -> msg -> model -> ( model, Cmd msg, Cmd Global.Msg )
    , subscriptions : Global.Model -> model -> Sub msg
    , view : Global.Model -> Timeline model -> Document msg
    }
    -> Page flags model msg
component page =
    { init =
        \global flags ->
            let
                ( model, cmd, globalCmd ) =
                    page.init global flags
            in
            ( toTimeline model, cmd, globalCmd )
    , update =
        \global msg modelTimeline ->
            let
                ( model, cmd, globalCmd ) =
                    page.update global msg (Timeline.value modelTimeline)
            in
            ( model |> toTimeline, cmd, globalCmd )
    , view = \global modelTimeline -> page.view global modelTimeline
    , subscriptions = \global modelTimeline -> page.subscriptions global (Timeline.value modelTimeline)
    }
