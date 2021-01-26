module App.Style exposing (..)

import Element exposing (Element)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border

colors =
    { blue = (Element.rgb255 50 74 94)
    , teal = (Element.rgb255 76 219 196)
    , grey = (Element.rgb255 218 218 218)
    , darkgrey = (Element.rgb255 44 44 44)
    }

logo: Int -> Element msg
logo height =
    Element.image
        [ Element.height (Element.px height)
        ]
    { src = "img/hop.svg"
    , description = "Hoppr the frog"
    }

header: Element msg
header =
    Element.wrappedRow
        [ Element.spacing 7
        ]
        [ (logo 40)
        , Element.el
            [ Font.bold
            , Font.color (colors.blue)
            , Font.size 32
            ]
              ( Element.text "Hoppr" )
        ]

navRight: msg -> Element msg
navRight msg =
    Element.el
        [ Element.height Element.fill
        ]
        (Element.image
              [ Element.height ( Element.px 100)
              , Element.alignRight
              , Element.centerY
              , (Events.onClick msg)
              ]
              { src = "./img/nav-right.svg"
              , description = "right nav arrow"
              })

navLeft: msg -> Element msg
navLeft msg =
    Element.el
        []
        (Element.image
              [ Element.height ( Element.px 100 )
              , Element.alignLeft
              , (Events.onClick msg)
              ]
              { src = "./img/nav-left.svg"
              , description = "left nav arrow"
              })

pause: msg -> Element msg
pause msg =
    Element.el
        []
        (Element.image
              [ Element.height ( Element.px 50 )
              , Element.centerX
              , Element.centerY
              , (Events.onClick msg)
              ]
              { src = "./img/pause.svg"
              , description = "pause"
              })

play: msg -> Element msg
play msg =
    Element.el
        []
        (Element.image
              [ Element.height ( Element.px 50 )
              , Element.centerX
              , Element.centerY
              , (Events.onClick msg)
              ]
              { src = "./img/play.svg"
              , description = "play"
              })

button: Int -> String -> String -> Element msg
button size href labelText =
    Input.button
              [ Font.size (size)
              , Font.color colors.blue
              , Background.color colors.grey
              , Border.rounded 10
              , Border.width 2
              , Element.padding 7
              , Element.focused
                  [ Background.color colors.teal ]
              ]
                { onPress = Nothing
                , label = (Element.link [] { url = href, label = Element.text labelText })
                }

slider: (Int -> msg) -> Int -> Int -> Int -> Element msg
slider changeMsg sliderValue min max =
    Input.slider
        [ Element.height <| Element.px 30
        -- Track
        , Element.behindContent
            <| Element.el
                [ Element.height <| Element.px 2
                , Element.width (Element.fill)
                , Element.centerY
                , Element.centerX
                , Background.color colors.blue
                , Border.rounded 2
                ]
                Element.none
        ]
        { onChange = (\f -> (round f) |> changeMsg)
        , label = Input.labelRight [] <| Element.text <| String.fromInt sliderValue
        , min = (toFloat min)
        , max = (toFloat max)
        , step = (Just 1)
        , value = (toFloat sliderValue)
        , thumb = Input.defaultThumb
        }
