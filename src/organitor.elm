module Organitor exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Markdown
import String


-- MODEL


type alias Model =
    { docTitle : String
    , document : Content
    , headingWithEditor : Maybe Content
    , newHeadingText : String
    , activeHeading : Maybe Content
    }


{-| TODO: Fix this type since there is only one version
-}
type alias Content =
    { title : String
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
    { docTitle = ""
    , document =
        { title = "Doc Title"
        , copy = ""
        , children =
            Children
                [ { title = "Heading 1"
                  , copy = "_lol some copy_"
                  , children =
                        Children
                            [ { title = "Heading 1.1.1"
                              , copy = "We needa clear dis up"
                              , children = Children []
                              }
                            ]
                  }
                , { title = "Heading 2"
                  , copy = "_lol some other copy_"
                  , children = Children []
                  }
                ]
        }
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


addNewHeading : Content -> Content -> Content -> Content
addNewHeading parentHeading newHeading document =
    let
        addToChildren (Children children) content =
            Children (children ++ ([ content ]))
    in
        -- Walk tree to find target heading
        if document == parentHeading then
            -- Found the heading, so add this new content to the child
            { document
                | children = addToChildren document.children newHeading
            }
        else
            { document
                | children =
                    mapChildren
                        (addNewHeading parentHeading newHeading)
                        document.children
            }


addCopyToHeading : Content -> String -> Content -> Content
addCopyToHeading parentHeading newCopy document =
    if document == parentHeading then
        { document | copy = newCopy }
    else
        { document
            | children =
                mapChildren
                    (addCopyToHeading parentHeading newCopy)
                    document.children
        }


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
                    Content model.newHeadingText "" (Children [])
            in
                { model
                    | document =
                        addNewHeading parentHeading newHeading model.document
                }

        EditCopy heading ->
            { model | activeHeading = Just heading }

        UpdateCopy heading copy ->
            { model
                | document = addCopyToHeading heading copy model.document
                , activeHeading =
                    case model.activeHeading of
                        Just content ->
                            Just { content | copy = copy }

                        Nothing ->
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
        [ li []
            [ if showEditor then
                addEditorInput
              else
                openEditorControl
            ]
        ]


tableOfContents : List ( String, String ) -> Model -> Html Msg
tableOfContents parentStyles { document, headingWithEditor, newHeadingText } =
    let
        renderHeading content =
            li []
                [ a [ href "#", onClick (EditCopy content) ] [ text content.title ]
                , ul []
                    ((walkChildren content.children)
                        ++ (newHeadingRevealLink content
                                headingWithEditor
                                newHeadingText
                           )
                    )
                ]

        walkChildren (Children children) =
            List.map renderHeading children
    in
        ul [] [ renderHeading document ]


editorView : List ( String, String ) -> Model -> Html Msg
editorView parentStyles { document, activeHeading } =
    let
        styles =
            List.concat [ parentStyles, [] ]

        editor =
            case activeHeading of
                Just heading ->
                    [ textarea
                        [ Attrs.value heading.copy
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
                        ([ (Markdown.toHtml [] content.copy) ]
                            ++ (renderChildren (depth + 1) content.children)
                        )
                    ]
            else
                (renderHeading depth content.title)

        renderChildren depth (Children children) =
            List.map
                (\child ->
                    div []
                        ([ renderHeading depth child.title ]
                            ++ [ Markdown.toHtml [] child.copy ]
                            ++ (renderChildren (depth + 1) child.children)
                        )
                )
                children
    in
        section [ class "renderer", style styles ]
            [ (renderedDocument 0 model.document) ]
