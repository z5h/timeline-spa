module Pages exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Generated.Route as Route exposing (Route)
import Global
import Page exposing (Bundle, Document)
import Pages.Docs
import Pages.NotFound
import Pages.Top
import Timeline exposing (Timeline)



-- TYPES


type Model
    = Top_Model Pages.Top.Model
    | Docs_Model Pages.Docs.Model
    | NotFound_Model Pages.NotFound.Model


type Msg
    = Top_Msg Pages.Top.Msg
    | Docs_Msg Pages.Docs.Msg
    | NotFound_Msg Pages.NotFound.Msg



-- PAGES


type alias UpgradedPage flags model msg =
    { init : flags -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , update : msg -> model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , bundle : Timeline model -> Global.Model -> Bundle Msg
    }


type alias UpgradedPages =
    { top : UpgradedPage Pages.Top.Flags Pages.Top.Model Pages.Top.Msg
    , docs : UpgradedPage Pages.Docs.Flags Pages.Docs.Model Pages.Docs.Msg
    , notFound : UpgradedPage Pages.NotFound.Flags Pages.NotFound.Model Pages.NotFound.Msg
    }


pages : UpgradedPages
pages =
    { top = Pages.Top.page |> Page.upgrade Top_Model Top_Msg
    , docs = Pages.Docs.page |> Page.upgrade Docs_Model Docs_Msg
    , notFound = Pages.NotFound.page |> Page.upgrade NotFound_Model NotFound_Msg
    }



-- INIT


init : Route -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
init route =
    case route of
        Route.Top ->
            pages.top.init ()

        Route.Docs ->
            pages.docs.init ()

        Route.NotFound ->
            pages.notFound.init ()



-- UPDATE


update : Msg -> Model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( Top_Msg msg, Top_Model model ) ->
            pages.top.update msg model

        ( Docs_Msg msg, Docs_Model model ) ->
            pages.docs.update msg model

        ( NotFound_Msg msg, NotFound_Model model ) ->
            pages.notFound.update msg model

        _ ->
            always ( bigModel, Cmd.none, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Timeline Model -> Global.Model -> Bundle Msg
bundle timelineModel =
    case Timeline.value timelineModel of
        Top_Model model ->
            toTimelineBundle timelineModel bundleModelTop pages.top.bundle model

        Docs_Model model ->
            toTimelineBundle timelineModel bundleModelDocs pages.docs.bundle model

        NotFound_Model model ->
            toTimelineBundle timelineModel bundleModelNotFound pages.notFound.bundle model


toTimelineBundle :
    Timeline Model
    -> (Model -> Maybe pageModel)
    -> (Timeline pageModel -> Global.Model -> Bundle Msg)
    -> pageModel
    -> Global.Model
    -> Bundle Msg
toTimelineBundle timeline toMaybe pageBundle pageModel global =
    let
        continuousPageTimeline : Timeline pageModel
        continuousPageTimeline =
            timeline
                |> Timeline.map toMaybe
                |> Timeline.withDefault pageModel
    in
    pageBundle continuousPageTimeline global


bundleModelTop : Model -> Maybe Pages.Top.Model
bundleModelTop model =
    case model of
        Top_Model model_ ->
            Just model_

        _ ->
            Nothing


bundleModelDocs : Model -> Maybe Pages.Docs.Model
bundleModelDocs model =
    case model of
        Docs_Model model_ ->
            Just model_

        _ ->
            Nothing


bundleModelNotFound : Model -> Maybe Pages.NotFound.Model
bundleModelNotFound model =
    case model of
        NotFound_Model model_ ->
            Just model_

        _ ->
            Nothing


view : Timeline Model -> Global.Model -> Document Msg
view model =
    bundle model >> .view


subscriptions : Timeline Model -> Global.Model -> Sub Msg
subscriptions model =
    bundle model >> .subscriptions
