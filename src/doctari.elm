module Doctari
    exposing
        ( Model
        , Msg
        , empty
        , update
        , view
        )

{-| This library exposes a component following the Elm Architecture that offers
an ordered, organizable live-Markdown document editor that can serialize its
contents to JSON.

That is certainly a mouthful, but the idea is to offer users to create documents
in a structured and organized way. Headings group related and supporting ideas
that can be re-organized or re-emphasized as the document evolves.

Again, since the component designs around the Elm Architecture, clients should
be able to weave it in to the existing Model/Update/View scaffolding of their
application.

Probably the most annoying part is:

- setting up a model
- grafting the component action `Msg`s into that of your application

For the first issue, Doctari exposes an `empty` value that offers a good
starting point for the component's states with sane defaults.

For the second issue, I'd like to come up with a way to keep `Msg` values with
no relevance to the outside world internally contained. Until I figure that
out, they are emitted, mapped, and rerouted back into the component by the
consuming app:

    type Msg = DoctariMsg Doctari.Msg

    update msg model =
        case msg of
            DoctariMsg msg ->
                { model |
                    doctariModel =
                        Doctari.update msg model.doctariModel
                }

    view : Model -> Html Msg
    view model =
        div [] [ App.map DoctariMsg Doctari.view ]

You can see more complete usage in the [example][].

TODO:

- Find the most intelligent way to ask for a serialized version of the
current value of the component's state
- Find a decent way to create a component with an initial serialized state
    (useful for rendering a version of the document stored on the server)

[example]: https://github.com/thedahv/doctari/tree/master/examples/basic/main.elm

# Model
@docs Model
@docs empty

# Update
@docs Msg
@docs update

# View
@docs view
-}

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Markdown
import Random
import String


-- MODEL


{-| The most important part of Doctari's state is the representation of its
contents. The `document` field models a heading's ID, title, content, and
list of children (which also happen to have the same structure).

The rest of the fields represent changes in the organization of the headings
within the document structure.
-}
type Model
    = Model
        { document : Content
        , newHeadingParentId : Maybe Int
        , newHeadingText : String
        , activeId : Maybe Int
        , activeIndex : Maybe Int
        , idGenerator : Random.Generator Int
        , idSeed : Random.Seed
        , serialized : String
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


filterChildren : (Content -> Bool) -> Children -> Children
filterChildren fn (Children children) =
    Children (List.filter fn children)


appendChildren : Children -> List Content -> Children
appendChildren (Children children) more =
    Children (children ++ more)


{-| Convert a document into a JSON-representation of the document. Useful for
saving an in-progress document or sending it outside of the app for further
processing.

    let
      doc = Content "Document Title" "" (Children
        [ Content "First Heading" "" (Children []) ]
      )
    in
      encodeDocument doc ==
        "{ \"title\": \"Document Title\", \"copy\": \"\", \"children\": " ++
          "[ { \"title\": \"First Heading\", \"copy\": \"\", \"children\": " ++
          " [] } ] }"
-}
encodeDocument : Content -> Encode.Value
encodeDocument content =
    let
        encodedChildren : Children -> List Encode.Value
        encodedChildren (Children children) =
            List.map encodeDocument children
    in
        Encode.object
            [ ( "id", Encode.int content.id )
            , ( "title", Encode.string content.title )
            , ( "copy", Encode.string content.copy )
            , ( "children", Encode.list (encodedChildren content.children) )
            ]


{-| The empty document has a sample document title that the user can change,
and no children. Users of this component should initialize it with this value.
-}
empty : Model
empty =
    Model
        { document =
            { id = 0
            , title = "Document Title"
            , copy = ""
            , children = Children []
            }
        , newHeadingParentId = Nothing
        , newHeadingText = ""
        , activeId = Just 0
        , activeIndex = Just 0
        , idGenerator =
            Random.int 0 Random.maxInt
            -- 6787 is just my birthday :) I thought about using the current time,
            -- but that's more work than we need.
        , idSeed = Random.initialSeed 6787
        , serialized = ""
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


getParentOfId : Int -> Content -> Maybe Content
getParentOfId id document =
    let
        matchingChildren =
            case filterChildren (\child -> child.id == id) document.children of
                Children children ->
                    children
    in
        if 0 < List.length matchingChildren then
            Just document
        else
            case document.children of
                Children [] ->
                    Nothing

                Children children ->
                    List.filterMap (getParentOfId id) children
                        |> List.head


moveChildToIndex : Int -> Content -> Children -> Children
moveChildToIndex index movedHeading (Children children) =
    let
        cleaned =
            List.filter
                (\child -> not (child.id == movedHeading.id))
                children

        before =
            List.take index cleaned

        after =
            List.drop index cleaned
    in
        Children (before ++ [ movedHeading ] ++ after)


moveToIndex : Int -> Content -> Int -> Content -> Content
moveToIndex parentId heading index content =
    let
        hasChildren =
            case content.children of
                Children children ->
                    0 < List.length children

        rearranged =
            if content.id == parentId then
                content.children |> moveChildToIndex index heading
            else
                content.children
                    |> mapChildren (moveToIndex parentId heading index)
    in
        { content | children = rearranged }


promoteHeading : Content -> Content -> Content -> Content -> Content
promoteHeading parent grandparent heading document =
    document
        |> removeHeading parent.id heading.id
        |> addNewHeading grandparent.id heading



-- UPDATE


{-| Msg represents all the user actions to request changes to the document
content or structure
-}
type Msg
    = UpdateTitle Content String
    | AddHeadingEditorToParent Int
    | CancelNewHeading
    | NewHeadingText String
    | SaveNewHeadingToParent Int
    | EditCopy Int Content
    | UpdateCopy Content String
    | MoveToIndex Int Int
    | PromoteToParent Int
    | SerializeModel
    | DeleteSection Int


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


removeHeading : Int -> Int -> Content -> Content
removeHeading parentHeadingId headingId document =
    let
        removeFromChildren =
            filterChildren (\{ id } -> not (id == headingId))

        removeFromParent content =
            if content.id == parentHeadingId then
                { content | children = removeFromChildren content.children }
            else
                case content.children of
                    Children [] ->
                        content

                    _ ->
                        { content
                            | children =
                                mapChildren removeFromParent content.children
                        }
    in
        removeFromParent document


updateHeadingTitle : Int -> String -> Content -> Content
updateHeadingTitle headingId title document =
    if document.id == headingId then
        { document | title = title }
    else
        { document
            | children =
                mapChildren
                    (updateHeadingTitle headingId title)
                    document.children
        }


addCopyToHeading : Int -> String -> Content -> Content
addCopyToHeading headingId newCopy document =
    if document.id == headingId then
        { document | copy = newCopy }
    else
        { document
            | children =
                mapChildren
                    (addCopyToHeading headingId newCopy)
                    document.children
        }


{-| update follows the standard pattern of producing a new version of the
document contents and/or structure based on user action.
-}
update : Msg -> Model -> Model
update msg (Model model) =
    Model <|
        case msg of
            UpdateTitle heading newTitle ->
                { model
                    | document =
                        updateHeadingTitle heading.id newTitle model.document
                }

            AddHeadingEditorToParent headingId ->
                { model | newHeadingParentId = Just headingId }

            CancelNewHeading ->
                { model
                    | newHeadingParentId = Nothing
                    , newHeadingText = ""
                }

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
                        , activeId = Just newHeading.id
                        , newHeadingText = ""
                        , newHeadingParentId = Nothing
                        , document =
                            addNewHeading
                                parentHeadingId
                                newHeading
                                model.document
                    }

            EditCopy idxWithinParent heading ->
                { model
                    | activeId = Just heading.id
                    , activeIndex = Just idxWithinParent
                }

            UpdateCopy heading copy ->
                { model
                    | document =
                        addCopyToHeading heading.id copy model.document
                }

            MoveToIndex headingId index ->
                let
                    parent =
                        getParentOfId headingId model.document

                    heading =
                        getById headingId model.document

                    maxMove =
                        case parent of
                            Just p ->
                                case p.children of
                                    Children children ->
                                        Just (List.length children)

                            _ ->
                                Nothing

                    shouldMove =
                        case maxMove of
                            Just max ->
                                index >= 0 && index <= max

                            Nothing ->
                                False
                in
                    case ( parent, heading, shouldMove ) of
                        ( Just p, Just h, True ) ->
                            { model
                                | document =
                                    moveToIndex
                                        p.id
                                        h
                                        index
                                        model.document
                                , activeIndex = Just index
                            }

                        _ ->
                            model

            PromoteToParent headingId ->
                let
                    parent =
                        getParentOfId
                            headingId
                            model.document

                    grandparent =
                        case parent of
                            Just p ->
                                getParentOfId p.id model.document

                            Nothing ->
                                Nothing

                    heading =
                        getById headingId model.document
                in
                    case ( parent, grandparent, heading ) of
                        ( Just parent, Just grandparent, Just heading ) ->
                            { model
                                | document =
                                    model.document
                                        |> promoteHeading
                                            parent
                                            grandparent
                                            heading
                            }

                        _ ->
                            model

            DeleteSection headingId ->
                -- TODO Somehow convince the consuming app to prompt the user to
                -- confirm and feed that back in as a new action.
                let
                    parent =
                        getParentOfId headingId model.document
                in
                    case parent of
                        Just p ->
                            { model
                                | document =
                                    model.document
                                        |> removeHeading
                                            p.id
                                            headingId
                            }

                        Nothing ->
                            -- TODO Let the user know the world is ending...
                            model

            SerializeModel ->
                { model
                    | serialized =
                        Encode.encode 2 (encodeDocument model.document)
                }



-- VIEW


{-| view renders a document based on its contents and structure, as well as
user controls to manipulate the document
-}
view : Model -> Html Msg
view model =
    let
        paneStyles =
            [ ( "display", "inline-block" ), ( "vertical-align", "top" ) ]
    in
        div [ class "doctari wrapper" ]
            [ header [ style [ ( "text-align", "center" ) ] ] []
            , div [ style [] ]
                [ tableOfContents (paneStyles ++ [ ( "width", "20%" ) ]) model
                , editorView (paneStyles ++ [ ( "width", "26.67%" ) ]) model
                , rendererView (paneStyles ++ [ ( "width", "53.33%" ) ]) model
                ]
              -- , div [] [ text <| toString model ]
            , div []
                [ h3 [] [ text "Serialized" ]
                , button [ onClick SerializeModel ] [ text "Serialize" ]
                , serializedView model
                ]
            ]


newHeadingRevealLink :
    ( String, String )
    -> Content
    -> Maybe Int
    -> String
    -> List (Html Msg)
newHeadingRevealLink headingColor parentHeading newHeadingParentId newHeadingText =
    let
        showEditor =
            case newHeadingParentId of
                Just targetId ->
                    parentHeading.id == targetId

                _ ->
                    False

        openEditorMsg =
            AddHeadingEditorToParent parentHeading.id

        openEditorControl =
            a
                [ href "#"
                , onClick openEditorMsg
                , style [ headingColor ]
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
tableOfContents parentStyles (Model model) =
    let
        { document, newHeadingParentId, newHeadingText, activeId } =
            model

        headingStyles : ( String, String ) -> Content -> List ( String, String )
        headingStyles colorStyle heading =
            List.concat
                [ [ colorStyle ]
                , case activeId of
                    Just id ->
                        if id == heading.id then
                            [ ( "font-weight", "700" ) ]
                        else
                            []

                    _ ->
                        []
                ]

        depthColor : Int -> ( String, String )
        depthColor depth =
            ( "color"
            , case depth of
                0 ->
                    "black"

                1 ->
                    "#2ecc71"

                2 ->
                    "#3498db"

                3 ->
                    "#e67e22"

                4 ->
                    "#d35400"

                _ ->
                    "#8e44ad"
            )

        renderHeading : Int -> Int -> Content -> Html Msg
        renderHeading depth idxWithinParent content =
            li []
                [ a
                    [ href "#"
                    , onClick (EditCopy idxWithinParent content)
                    , style (headingStyles (depthColor depth) content)
                    ]
                    [ text content.title ]
                , ul []
                    ((walkChildren (depth + 1) content.children)
                        ++ (newHeadingRevealLink (depthColor depth)
                                content
                                newHeadingParentId
                                newHeadingText
                           )
                    )
                ]

        walkChildren : Int -> Children -> List (Html Msg)
        walkChildren depth (Children children) =
            List.indexedMap (renderHeading depth) children
    in
        div [ style parentStyles ]
            [ ul [] [ renderHeading 0 0 document ]
            ]


editorView : List ( String, String ) -> Model -> Html Msg
editorView parentStyles (Model { document, activeId, activeIndex }) =
    let
        activeHeading =
            case activeId of
                Just id ->
                    getById id document

                _ ->
                    Nothing

        orderingStyles =
            [ ( "padding", "0 4px" ) ]

        editor =
            case ( activeHeading, activeIndex ) of
                ( Just heading, Just index ) ->
                    [ p []
                        [ input
                            [ Attrs.value heading.title
                            , onInput (\text -> UpdateTitle heading text)
                            , style
                                [ ( "font-size", "1.5em" )
                                , ( "width", "90%" )
                                ]
                            ]
                            []
                        ]
                    , p []
                        [ a
                            [ href "#"
                            , style orderingStyles
                            , onClick (MoveToIndex heading.id (index - 1))
                            ]
                            [ text "Move Up" ]
                        , a
                            [ href "#"
                            , style orderingStyles
                            , onClick (MoveToIndex heading.id (index + 1))
                            ]
                            [ text "Move Down" ]
                        , a
                            [ href "#"
                            , style orderingStyles
                            , onClick (PromoteToParent heading.id)
                            ]
                            [ text "Promote Heading" ]
                        , a
                            [ href "#"
                            , onClick (DeleteSection heading.id)
                            ]
                            [ text "Delete Section" ]
                        ]
                    , textarea
                        [ Attrs.value heading.copy
                        , onInput (\text -> UpdateCopy heading text)
                        , style [ ( "width", "90%" ), ( "min-height", "30em" ) ]
                        ]
                        []
                    ]

                _ ->
                    []
    in
        div [ style parentStyles ]
            [ section [ class "editor" ] editor
            ]


rendererView : List ( String, String ) -> Model -> Html Msg
rendererView parentStyles (Model model) =
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
        div [ style parentStyles ]
            [ section
                [ class "renderer" ]
                [ renderedDocument 0 model.document ]
            ]


serializedView : Model -> Html Msg
serializedView (Model model) =
    div
        [ style [ ( "font-family", "Courier, sans-serif" ) ] ]
        [ text model.serialized ]
