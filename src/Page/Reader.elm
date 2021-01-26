module Page.Reader exposing (Msg, Model, init, update, view, subscriptions)

import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Time

import App.Style as Style

type Msg
    = TextChange String
    | WidthChange Int
    | SpeedChange Int
    | Pause Bool
    | NavLeft
    | NavRight
    | Tick

-- SUBS

subscriptions: (Msg -> msg) -> Model -> Sub msg
subscriptions return model =
    Time.every
        (toFloat model.viewInterval)
        (\_ -> (return Tick))

-- UPDATE

type State
    = Reading
    | Paused
    | Input

type alias Words = List (Element Msg)

type alias Model =
    { appKey: Nav.Key
    , text: String
    , state: State
    , width: Int
    , length: Int
    , page: Maybe Words
    , viewInterval: Int
    , wordBank: Maybe Words
    }

init: Nav.Key -> Model
init key =
    { appKey = key
    , text = ""
    , state = Input
    , width = 2
    , length = 5
    , page = Nothing
    , viewInterval = 1000
    , wordBank = Nothing
    }

initWords: String -> Maybe Words
initWords text =
    Just <| (List.map Element.text (String.words text))

incrementPage: Maybe Words -> Int -> Int -> (Maybe Words,Maybe Words)
incrementPage mBank width length =
    case mBank of
        Nothing ->
            (Nothing,Nothing)
        Just bank ->
            let
                size = (width*length)
                ns = (List.take size bank)
                nb = (List.drop size bank)
            in
                (Just ns,Just nb)

incrementLine: Maybe Words -> Int -> Int -> (Maybe Words,Maybe Words)
incrementLine mBank width length =
    case mBank of
        Nothing ->
            (Nothing,Nothing)
        Just bank ->
            (incrementPage (Just <| (List.drop width bank)) width length)

pushPage: Maybe Words -> Maybe Words -> Maybe Words
pushPage mPage mBank =
    case (mPage,mBank) of
        (Just page, Just bank) ->
            Just <| (List.append page bank)
        (_,_) ->
            mBank

nop: (Model -> return) -> Model -> (return, Cmd msg)
nop return model =
    ( (return model), Cmd.none )

update: (Model -> return) -> Msg -> Model -> (return, Cmd msg)
update return msg model =
    case msg of
       
        TextChange newtext ->
            let
                newmodel = { model | text = newtext }
            in
            ( (return newmodel)
            , Cmd.none
            )

        WidthChange width ->
            let
                resetBank = pushPage model.page model.wordBank
                (mPage,mBank) = incrementPage resetBank model.width model.length
            in
            ( (return { model
                          | width = width
                          , wordBank = mBank
                          , page = mPage
                      })
            , Cmd.none
            )

        SpeedChange interval ->
            ( (return { model | viewInterval = interval })
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
                Paused ->
                    (nop return model)

        NavRight ->
            case model.state of
                Input ->
                    let
                        (mPage,mBank) = incrementPage (initWords model.text) model.width model.length
                    in
                    ( (return { model
                                  | state = Reading
                                  , wordBank = mBank
                                  , page = mPage
                              })
                    , Cmd.none
                    )
                Reading ->
                    let
                        (mPage,mBank) = incrementPage model.wordBank model.width model.length
                    in
                    ( (return { model
                                  | wordBank = mBank
                                  , page = mPage
                              })
                    , Cmd.none
                    )
                Paused ->
                    (nop return model)

        Pause p ->
            if (p) then
                ( (return { model | state = Paused })
                , Cmd.none
                )
            else
                ( (return { model | state = Reading })
                , Cmd.none
                )

        Tick ->
            case model.state of
                Input ->
                    (nop return model)
                Paused ->
                    (nop return model)
                Reading ->
                    let
                        resetBank = pushPage model.page model.wordBank
                        (mPage,mBank) = incrementLine resetBank model.width model.length
                    in
                        ((return { model
                                      | wordBank = mBank
                                      , page = mPage
                                  })
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

viewPaused: Model -> Element Msg
viewPaused model =
    (Style.logo 150)

viewReader: Model -> Element Msg -> Element Msg
viewReader model icon =
    let
        currentPage = Maybe.withDefault [ (Element.text "Debug Error: No words in scene") ]  model.page
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
            , (viewPage currentPage model.width)
            , Style.navRight NavRight
            ]
        , Element.column
            [ Element.width <| ( Element.fill |> Element.maximum 500)
            , Element.height <| Element.fill
            , Element.centerX
            , Element.spacing 10
            ]
            [ (Style.slider WidthChange model.width 2 20)
            , (Style.slider SpeedChange model.viewInterval 100 5000)
            , icon
            ]
        ]

viewPage: Words -> Int -> Element Msg
viewPage page width =
    Element.column
        [ Element.width <| ( Element.fill |> Element.maximum 500 )
        , Element.height <| Element.fill
        , Element.centerX
        , Element.centerY
        ]
        (makePageRows page width)

makePageRows: Words -> Int -> List (Element Msg)
makePageRows bank width =
    let
        makeWordRow ws c =
            ws |> Element.row
                  [ Element.spacing 5
                  , Element.centerX
                  , Font.size 32
                  , Font.color c
                  ]

        makeRowsInner pageWords acc =
            case pageWords of
                [] ->
                    (List.reverse acc)
                _ ->
                    makeRowsInner (List.drop width pageWords) ((makeWordRow (List.take width pageWords) Style.colors.grey)::acc)
    in
    (makeRowsInner (List.drop width bank) [(makeWordRow (List.take width bank) Style.colors.darkgrey)])

view: Model -> Element Msg
view model =
    case model.state of
        Input ->
            (viewTextInputBox model)
        Reading ->
            (viewReader model (Style.pause (Pause True)))
        Paused ->
            (viewReader model (Style.play (Pause False)))
