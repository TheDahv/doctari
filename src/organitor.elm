module Organitor exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Markdown
import Random
import String


-- MODEL


type alias Model =
    { document : Content
    , newHeadingParentId : Maybe Int
    , newHeadingText : String
    , activeID : Maybe Int
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
    , activeID = Just 0
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
moveToIndex parentID heading index content =
    let
        hasChildren =
            case content.children of
                Children children ->
                    0 < List.length children

        rearranged =
            if content.id == parentID then
                content.children |> moveChildToIndex index heading
            else
                content.children
                    |> mapChildren (moveToIndex parentID heading index)
    in
        { content | children = rearranged }


promoteHeading : Content -> Content -> Content -> Content -> Content
promoteHeading parent grandparent heading document =
    document
        |> removeHeading parent.id heading.id
        |> addNewHeading grandparent.id heading



-- UPDATE


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

        EditCopy idxWithinParent heading ->
            { model
                | activeID = Just heading.id
                , activeIndex = Just idxWithinParent
            }

        UpdateCopy heading copy ->
            { model
                | document =
                    addCopyToHeading heading.id copy model.document
            }

        MoveToIndex headingID index ->
            let
                parent =
                    getParentOfId headingID model.document

                heading =
                    getById headingID model.document

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

        PromoteToParent headingID ->
            let
                parent =
                    getParentOfId
                        headingID
                        model.document

                grandparent =
                    case parent of
                        Just p ->
                            getParentOfId p.id model.document

                        Nothing ->
                            Nothing

                heading =
                    getById headingID model.document
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

        SerializeModel ->
            { model
                | serialized =
                    Encode.encode 2 (encodeDocument model.document)
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        paneStyles =
            [ ( "display", "inline-block" ), ( "vertical-align", "top" ) ]
    in
        div [ class "organitor wrapper" ]
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
                , div
                    [ style [ ( "font-family", "Courier, sans-serif" ) ]
                    ]
                    [ text model.serialized ]
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
        renderHeading idxWithinParent content =
            li []
                [ a
                    [ href "#"
                    , onClick (EditCopy idxWithinParent content)
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
            List.indexedMap renderHeading children
    in
        div [ style parentStyles ]
            [ ul [] [ renderHeading 0 document ]
            ]


editorView : List ( String, String ) -> Model -> Html Msg
editorView parentStyles { document, activeID, activeIndex } =
    let
        activeHeading =
            case activeID of
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
        div [ style parentStyles ]
            [ section
                [ class "renderer" ]
                [ renderedDocument 0 model.document ]
            ]
