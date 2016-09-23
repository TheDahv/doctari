module Organitor exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Markdown
import String


-- MODEL


type alias Model =
    { docTitle : String
    , structure : List Content
    , headingWithEditor : Maybe Content
    , newHeadingText : String
    , activeHeading : Maybe Content
    }


empty : Model
empty =
    { docTitle = ""
    , structure =
        [ Heading "Heading 1.1"
            "_hello world_"
            [ Heading "Heading 1.1.1" "" [] ]
        , Heading "Heading 1.2"
            ""
            [ Heading "Heading 1.2.1" "" [] ]
        , Heading "Heading 1.3"
            ""
            [ Heading "Heading 1.3.1" "" [] ]
        ]
    , headingWithEditor = Nothing
    , newHeadingText = ""
    , activeHeading = Nothing
    }



-- UPDATE


type Msg
    = UpdateTitle String
    | NewHeadingEditor Content
    | NewHeadingText String
    | AddHeading Content
    | EditCopy Content
    | UpdateCopy Content String


{-| TODO: Fix this type since there is only one version
-}
type Content
    = Heading String String (List Content)


addNewHeading : List Content -> Content -> Content -> List Content
addNewHeading document parentHeading newHeading =
    -- Walk tree to find target heading
    document
        |> List.map
            (\content ->
                case content of
                    Heading title copy children ->
                        if content == parentHeading then
                            -- Found the heading. Add the new heading to its subheadings
                            Heading title copy (children ++ [ newHeading ])
                        else
                            -- Ok, recurse through each child until we find it
                            Heading title copy (addNewHeading children parentHeading newHeading)
            )


addCopyToHeading : List Content -> Content -> String -> List Content
addCopyToHeading document parentHeading newCopy =
    document
        |> List.map
            (\content ->
                case content of
                    Heading title copy children ->
                        if content == parentHeading then
                            Heading title newCopy children
                        else
                            Heading title copy (addCopyToHeading children parentHeading newCopy)
            )


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
                    Heading model.newHeadingText "" []
            in
                { model
                    | structure =
                        addNewHeading model.structure
                            parentHeading
                            newHeading
                }

        EditCopy heading ->
            { model | activeHeading = Just heading }

        UpdateCopy heading copy ->
            { model
                | structure = addCopyToHeading model.structure heading copy
                , activeHeading =
                    case model.activeHeading of
                        Just (Heading title _ children) ->
                            Just (Heading title copy children)

                        _ ->
                            Nothing
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        paneStyles =
            [ ( "flex-grow", "2" ) ]
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
              -- , div [] [ text <| toString model ]
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
            Heading title copy _ ->
                [ li []
                    [ if showEditor then
                        addEditorInput
                      else
                        openEditorControl
                    ]
                ]


tableOfContents : List ( String, String ) -> Model -> Html Msg
tableOfContents parentStyles { structure, headingWithEditor, newHeadingText } =
    let
        renderHeading heading =
            case heading of
                Heading title copy children ->
                    Just
                        (li []
                            [ a [ onClick (EditCopy heading), href "#" ] [ text title ]
                            , ul []
                                (List.concat
                                    [ (walkTree children)
                                    , (newHeadingRevealLink heading headingWithEditor newHeadingText)
                                    ]
                                )
                            ]
                        )

        walkTree =
            List.filterMap renderHeading
    in
        div [] [ ul [] (walkTree structure) ]


editorView : List ( String, String ) -> Model -> Html Msg
editorView parentStyles { structure, activeHeading } =
    let
        styles =
            List.concat [ parentStyles, [] ]

        editor =
            case activeHeading of
                Just heading ->
                    case heading of
                        Heading _ copy _ ->
                            [ textarea
                                [ Attrs.value copy
                                , onInput (\text -> UpdateCopy heading text)
                                ]
                                []
                            ]

                Nothing ->
                    []
    in
        section [ class "editor", style styles ] editor


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
                            Heading title copy children ->
                                doc
                                    ++ [ renderHeading depth title ]
                                    ++ [ Markdown.toHtml [] copy ]
                                    ++ [ (renderedDocument children (depth + 1)) ]
                    )
                    []
                |> div [ style [ ( "overflow-y", "scroll" ) ] ]
    in
        section [ class "renderer", style styles ]
            [ h1
                [ style [ ( "text-align", "center" ) ] ]
                [ text model.docTitle ]
            , renderedDocument model.structure 1
            ]
