module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Doctari


main =
    App.beginnerProgram
        { view = view
        , update = update
        , model = model
        }



-- MODEL


type alias Model =
    { doctari : Doctari.Model
    }


model =
    Model Doctari.empty



-- Update


type Msg
    = DoctariMsg Doctari.Msg


update msg { doctari } =
    case msg of
        DoctariMsg msg ->
            { model | doctari = Doctari.update msg doctari }



-- View


view model =
    let
        doctariView =
            App.map DoctariMsg (Doctari.view model.doctari)
    in
        div []
            [ doctariView ]
