module Page.TextInput exposing (Msg, Model, init, update, view)

import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Input as Input

import App.Style as Style

type Msg
    = TextChange String
    | NavLeft
    | NavRight

-- UPDATE

type State
    = Reading
    | Input

type alias Model =
    { appKey: Nav.Key
    , text: String
    , state: State
    , width: Int
    , words: Maybe (List String)
    }

init: Nav.Key -> Model
init key =
    { appKey = key
    , text = ""
    , state = Input
    , width = 2
    , words = Nothing
    }

initWords: String -> List String
initWords text =
    (String.words text)

update: (Model -> rmod) -> Msg -> Model -> (rmod, Cmd msg)
update return msg model =
    case msg of
        TextChange newtext ->
            let
                newmodel = { model | text = newtext }
            in
            ( (return newmodel)
            , Cmd.none
            )
        NavLeft ->
            case model.state of
                Input ->
                    ( (return model)
                    , Nav.pushUrl model.appKey "/"
                    )
                Reading ->
                    ( (return { model | state = Input })
                    , Cmd.none
                    )
        NavRight ->
            case model.state of
                Input ->
                    ( (return { model
                                  | state = Reading
                                  , words = (Just <| initWords model.text)
                              })
                    , Cmd.none
                    )
                Reading ->
                    ( (return model)
                    , Cmd.none
                    )

-- VIEW

viewTextInputBox: Model -> Element Msg
viewTextInputBox model =
    Element.column
        [ Element.width <| Element.fill
        , Element.height <| Element.fill
        , Element.spacing 10
        , Element.padding 10
        ]
        [ Style.header
        , Element.row
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            ]
            [ Style.navLeft NavLeft
            , Input.multiline
                [ Element.width (Element.fill |> Element.maximum 750)
                , Element.height (Element.fill |> Element.maximum 750)
                , Element.centerX
                ]
                  { onChange = TextChange
                  , text = model.text
                  , placeholder = (Just (Input.placeholder [] (Element.text "enter some text to read...")))
                  , label = (Input.labelHidden "Reader Text")
                  , spellcheck = True
                  }
            , Style.navRight NavRight
            ]
        ]

viewReader: Model -> Element Msg
viewReader model =
    let
        words =
            case model.words of
                Just ws ->
                    ws
                Nothing ->
                    [""]
    in
    Element.column
        [ Element.width <| Element.fill
        , Element.height <| Element.fill
        , Element.spacing 10
        , Element.padding 10
        ]
        [ Style.header
        , Element.row
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            ]
            [ Style.navLeft NavLeft
            , (viewScene words model.width)
            ]
        ]

viewScene: List String -> Int -> Element Msg
viewScene words width =
    Element.column
        []
        (makeWordRows words width)

makeWordRows: List String -> Int -> List (Element Msg)
makeWordRows words width =
    let
        makeWordRow: List String -> Element Msg
        makeWordRow ws =
            Element.row
                [ Element.spacing 5 ]
                (List.map Element.text ws)
        makeRowsInner: List String -> List (Element Msg) -> List (Element Msg)
        makeRowsInner todo acc =
            case todo of
                [] ->
                    (List.reverse acc)
                _ ->
                    makeRowsInner (List.drop width todo) ((makeWordRow (List.take width todo))::acc)
    in
    (makeRowsInner words [])

view: Model -> Element Msg
view model =
    case model.state of
        Input ->
            (viewTextInputBox model)
        Reading ->
            (viewReader model)
