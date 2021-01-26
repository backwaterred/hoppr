module Page.Splash exposing (Msg, Model, init, view, update)

import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Events
import Element.Font as Font
import Url exposing (Url)

import App.Style as Style

type Msg
    = NavRight

-- UPDATE

type alias Model =
    { appKey: Nav.Key }

init: Nav.Key -> Model
init key =
    { appKey = key }

update: (Model -> rmod) -> Msg -> Model -> (rmod, Cmd msg)
update return msg model =
    case msg of
        NavRight ->
            ( (return model)
            , Nav.pushUrl model.appKey "/reader"
            )

-- VIEW

view: Model -> Element Msg
view model =
    Element.column
          [ Element.centerX
          , Element.centerY
          , Element.spacing 10
          ]
          [ Element.row
                []
                [(Style.logo 150)
                , Element.el
                    [ Font.bold
                    , Font.size 64
                    , Font.color (Style.colors.blue)
                    ]
                      ( Element.text "Hoppr!" )
                ,(Style.navRight NavRight)
                ]
          ]
