module Pages.Top exposing (Flags, Model, Msg, page)

import Html
import Html.Events
import Page exposing (Document, Page)
import Timeline exposing (Status(..), Timeline)


type alias Flags =
    ()


type alias Model =
    { a : Int
    , b : Int
    }


type Msg
    = IncrementA
    | IncrementB


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementA ->
            { model | a = model.a + 1 }

        IncrementB ->
            { model | b = model.b + 1 }


page : Page Flags Model Msg
page =
    Page.sandbox
        { init = { a = 0, b = 0 }
        , update = update
        , view = view
        }


view : Timeline Model -> Document Msg
view timeline =
    let
        value =
            Timeline.value timeline

        progressA =
            timeline |> Timeline.map .a |> Timeline.transition 5000

        progressB =
            timeline |> Timeline.map .b |> Timeline.transition 3000

        statusText progress =
            case progress of
                At t duration ->
                    "At " ++ String.fromInt t

                Transitioning from to progress_ ->
                    "Transitioning from " ++ String.fromInt from ++ " to " ++ String.fromInt to ++ " : " ++ String.fromFloat progress_
    in
    { title = "Top"
    , body =
        [ Html.text "Top"
        , Html.p [] [ Html.text <| statusText progressA ]
        , Html.p [] [ Html.text <| statusText progressB ]
        , Html.button [ Html.Events.onClick IncrementA ] [ Html.text "incrementA" ]
        , Html.button [ Html.Events.onClick IncrementB ] [ Html.text "incrementB" ]
        ]
    }
