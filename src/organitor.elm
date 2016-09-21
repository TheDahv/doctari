module Organitor exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import String


-- MODEL


type alias Model =
    { docTitle : String
    , structure : List Content
    , headingWithEditor : Maybe Content
    , newHeadingText : String
    }


empty : Model
empty =
    { docTitle = ""
    , structure =
        [ Heading "Heading 1.1"
            [ Heading "Heading 1.1.1" [] ]
        , Heading "Heading 1.2"
            [ Heading "Heading 1.2.1" [] ]
        , Heading "Heading 1.3"
            [ Heading "Heading 1.3.1" [] ]
        ]
    , headingWithEditor = Nothing
    , newHeadingText = ""
    }



-- UPDATE


type Msg
    = UpdateTitle String
    | NewHeadingEditor Content
    | NewHeadingText String
    | AddHeading Content


type Content
    = Heading String (List Content)
    | Copy String


addNewHeading : List Content -> Content -> Content -> List Content
addNewHeading document parentHeading newHeading =
    -- Walk tree to find target heading
    List.map
        (\content ->
            case content of
                Heading title children ->
                    if content == parentHeading then
                        -- Found the heading. Add the new heading to its subheadings
                        Heading title (children ++ [ newHeading ])
                    else
                        -- Ok, recurse through each child until we find it
                        Heading title (addNewHeading children parentHeading newHeading)

                _ ->
                    -- We don't care about this if it's not a heading
                    content
        )
        document


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTitle newTitle ->
            { model | docTitle = newTitle }

        NewHeadingEditor heading ->
            { model | headingWithEditor = Just heading }

        NewHeadingText newText ->
            { model | newHeadingText = newText }

        AddHeading parentHeading ->
            let
                newHeading =
                    Heading model.newHeadingText []
            in
                { model
                    | structure =
                        addNewHeading model.structure
                            parentHeading
                            newHeading
                }



-- VIEW


view : Model -> Html Msg
view model =
    let
        paneStyles =
            [ ( "flex-grow", "2" )
              {-
                 , ( "width", "50%" )
                 , ( "min-width", "50%" )
                 , ( "max-width", "50%" )
              -}
            ]
    in
        div [ class "organitor wrapper" ]
            [ header [ style [ ( "text-align", "center" ) ] ]
                [ input
                    [ placeholder "Document Title"
                    , Attrs.value model.docTitle
                    , onInput UpdateTitle
                    , style [ ( "width", "75%" ) ]
                    ]
                    []
                ]
            , div [ style [ ( "display", "flex" ) ] ]
                [ tableOfContents [ ( "flex-grow", "1" ) ] model
                , editorView paneStyles model
                , rendererView paneStyles model
                ]
            ]


newHeadingRevealLink : Content -> Maybe Content -> String -> List (Html Msg)
newHeadingRevealLink parentHeading headingWithEditor newHeadingText =
    let
        showEditor =
            case headingWithEditor of
                Just content ->
                    content == parentHeading

                _ ->
                    False

        openEditorMsg =
            NewHeadingEditor parentHeading

        openEditorControl =
            a
                [ href "#"
                , onClick openEditorMsg
                ]
                [ text "Add heading..." ]

        addEditorInput =
            div []
                [ input
                    [ placeholder "New Heading..."
                    , onInput NewHeadingText
                    ]
                    []
                , button
                    [ disabled (0 == String.length newHeadingText)
                    , onClick (AddHeading parentHeading)
                    ]
                    [ text "Save" ]
                ]
    in
        case parentHeading of
            Heading title _ ->
                [ li []
                    [ if showEditor then
                        addEditorInput
                      else
                        openEditorControl
                    ]
                ]

            _ ->
                []


tableOfContents : List ( String, String ) -> Model -> Html Msg
tableOfContents parentStyles { structure, headingWithEditor, newHeadingText } =
    let
        renderHeading heading =
            case heading of
                Heading title children ->
                    Just
                        (li []
                            [ text title
                            , ul []
                                (List.concat
                                    [ (walkTree children)
                                    , (newHeadingRevealLink heading headingWithEditor newHeadingText)
                                    ]
                                )
                            ]
                        )

                _ ->
                    Nothing

        walkTree =
            List.filterMap renderHeading
    in
        div [] [ ul [] (walkTree structure) ]


editorView : List ( String, String ) -> Model -> Html Msg
editorView parentStyles model =
    let
        styles =
            List.concat [ parentStyles, [] ]
    in
        section [ class "editor", style styles ] []


rendererView : List ( String, String ) -> Model -> Html Msg
rendererView parentStyles model =
    let
        styles =
            List.concat [ parentStyles, [] ]

        renderHeading depth copy =
            case depth of
                1 ->
                    h1 [] [ text copy ]

                2 ->
                    h2 [] [ text copy ]

                3 ->
                    h3 [] [ text copy ]

                4 ->
                    h4 [] [ text copy ]

                _ ->
                    h5 [] [ text copy ]

        renderedDocument document depth =
            document
                |> List.foldl
                    (\content doc ->
                        case content of
                            Heading title children ->
                                doc
                                    ++ [ renderHeading depth title ]
                                    ++ [ (renderedDocument children (depth + 1)) ]

                            _ ->
                                doc
                    )
                    []
                |> div []
    in
        section [ class "renderer", style styles ]
            [ h1
                [ style [ ( "text-align", "center" ) ] ]
                [ text model.docTitle ]
            , renderedDocument model.structure 1
            ]
