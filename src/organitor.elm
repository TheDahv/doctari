module Organitor exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Markdown
import Random
import String


-- MODEL


type alias Model =
    { document : Content
    , newHeadingParentId : Maybe Int
    , newHeadingText : String
    , activeID : Maybe Int
    , idGenerator : Random.Generator Int
    , idSeed : Random.Seed
    }


type alias Content =
    { id : Int
    , title : String
    , copy : String
    , children : Children
    }


type Children
    = Children (List Content)


mapChildren : (Content -> Content) -> Children -> Children
mapChildren fn (Children children) =
    Children (List.map fn children)


empty : Model
empty =
    { document =
        { id = 0
        , title = "Document Title"
        , copy = ""
        , children = Children []
        }
    , newHeadingParentId = Nothing
    , newHeadingText = ""
    , activeID = Nothing
    , idGenerator =
        Random.int 0 Random.maxInt
        -- 6787 is just my birthday :) I thought about using the current time,
        -- but that's more work than we need.
    , idSeed = Random.initialSeed 6787
    }


getById : Int -> Content -> Maybe Content
getById id content =
    if content.id == id then
        Just content
    else
        case content.children of
            -- Base case: At a child with no more children, and still no match
            Children [] ->
                Nothing

            Children children ->
                -- filterMap will drop any "Nothing" that comes up from the
                -- children, so we know this will boil down to at least one
                -- "Just content" or nothing at all.
                List.filterMap (getById id) children
                    |> List.head



-- UPDATE


type Msg
    = UpdateTitle Content String
    | AddHeadingEditorToParent Int
    | CancelNewHeading
    | NewHeadingText String
    | SaveNewHeadingToParent Int
    | EditCopy Content
    | UpdateCopy Content String


addNewHeading : Int -> Content -> Content -> Content
addNewHeading parentHeadingId newHeading document =
    let
        addToChildren (Children children) content =
            Children (children ++ [ content ])
    in
        -- Walk tree to find target heading
        if document.id == parentHeadingId then
            -- Found the heading, so add this new content to the child
            { document
                | children = addToChildren document.children newHeading
            }
        else
            { document
                | children =
                    mapChildren
                        (addNewHeading parentHeadingId newHeading)
                        document.children
            }


updateHeadingTitle : Int -> String -> Content -> Content
updateHeadingTitle headingID title document =
    if document.id == headingID then
        { document | title = title }
    else
        { document
            | children =
                mapChildren
                    (updateHeadingTitle headingID title)
                    document.children
        }


addCopyToHeading : Int -> String -> Content -> Content
addCopyToHeading headingID newCopy document =
    if document.id == headingID then
        { document | copy = newCopy }
    else
        { document
            | children =
                mapChildren
                    (addCopyToHeading headingID newCopy)
                    document.children
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTitle heading newTitle ->
            { model
                | document =
                    updateHeadingTitle heading.id newTitle model.document
            }

        AddHeadingEditorToParent headingID ->
            { model | newHeadingParentId = Just headingID }

        CancelNewHeading ->
            { model | newHeadingParentId = Nothing }

        NewHeadingText newText ->
            { model | newHeadingText = newText }

        SaveNewHeadingToParent parentHeadingId ->
            let
                ( newId, newSeed ) =
                    Random.step model.idGenerator model.idSeed

                newHeading =
                    Content newId model.newHeadingText "" (Children [])
            in
                { model
                    | idSeed = newSeed
                    , document =
                        addNewHeading
                            parentHeadingId
                            newHeading
                            model.document
                }

        EditCopy heading ->
            { model | activeID = Just heading.id }

        UpdateCopy heading copy ->
            { model
                | document =
                    addCopyToHeading heading.id copy model.document
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        paneStyles =
            [ ( "flex-grow", "2" ) ]
    in
        div [ class "organitor wrapper" ]
            [ header [ style [ ( "text-align", "center" ) ] ] []
            , div [ style [ ( "display", "flex" ) ] ]
                [ tableOfContents [ ( "flex-grow", "1" ) ] model
                , editorView paneStyles model
                , rendererView paneStyles model
                ]
            ]


newHeadingRevealLink : Content -> Maybe Int -> String -> List (Html Msg)
newHeadingRevealLink parentHeading newHeadingParentId newHeadingText =
    let
        showEditor =
            case newHeadingParentId of
                Just targetID ->
                    parentHeading.id == targetID

                _ ->
                    False

        openEditorMsg =
            AddHeadingEditorToParent parentHeading.id

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
                    , onClick (SaveNewHeadingToParent parentHeading.id)
                    ]
                    [ text "Save" ]
                , button
                    [ onClick CancelNewHeading ]
                    [ text "Cancel" ]
                ]
    in
        [ li []
            [ if showEditor then
                addEditorInput
              else
                openEditorControl
            ]
        ]


tableOfContents : List ( String, String ) -> Model -> Html Msg
tableOfContents parentStyles { document, newHeadingParentId, newHeadingText } =
    let
        renderHeading content =
            li []
                [ a
                    [ href "#"
                    , onClick (EditCopy content)
                    ]
                    [ text content.title ]
                , ul []
                    ((walkChildren content.children)
                        ++ (newHeadingRevealLink content
                                newHeadingParentId
                                newHeadingText
                           )
                    )
                ]

        walkChildren (Children children) =
            List.map renderHeading children
    in
        ul [] [ renderHeading document ]


editorView : List ( String, String ) -> Model -> Html Msg
editorView parentStyles { document, activeID } =
    let
        activeHeading =
            case activeID of
                Just id ->
                    getById id document

                Nothing ->
                    Nothing

        editor =
            case activeHeading of
                Just heading ->
                    [ p []
                        [ input
                            [ Attrs.value heading.title
                            , onInput (\text -> UpdateTitle heading text)
                            ]
                            []
                        ]
                    , textarea
                        [ Attrs.value heading.copy
                        , onInput (\text -> UpdateCopy heading text)
                        ]
                        []
                    ]

                Nothing ->
                    []
    in
        section [ class "editor", style parentStyles ] editor


rendererView : List ( String, String ) -> Model -> Html Msg
rendererView parentStyles model =
    let
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

        renderedDocument depth content =
            if depth == 0 then
                -- Render as Title
                div []
                    [ h1
                        [ class "title"
                        , style [ ( "text-align", "center" ) ]
                        ]
                        [ text content.title ]
                    , div []
                        ([ Markdown.toHtml [] content.copy ]
                            ++ renderChildren (depth + 1) content.children
                        )
                    ]
            else
                renderHeading depth content.title

        renderChildren depth (Children children) =
            List.map
                (\child ->
                    div []
                        ([ renderHeading depth child.title ]
                            ++ [ Markdown.toHtml [] child.copy ]
                            ++ renderChildren (depth + 1) child.children
                        )
                )
                children
    in
        section
            [ class "renderer", style parentStyles ]
            [ renderedDocument 0 model.document ]
