module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Organitor


main =
    App.beginnerProgram
        { view = view
        , update = update
        , model = model
        }



-- MODEL


type alias Model =
    { organitor : Organitor.Model
    }


model =
    Model Organitor.empty



-- Update


type Msg
    = OrganitorMsg Organitor.Msg


update msg { organitor } =
    case msg of
        OrganitorMsg msg ->
            { model | organitor = (Organitor.update msg organitor) }



-- View


view model =
    let
        organitorView =
            App.map OrganitorMsg (Organitor.view model.organitor)
    in
        div []
            [ organitorView ]
