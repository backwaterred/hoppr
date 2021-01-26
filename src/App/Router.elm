module App.Router exposing (..)

import Url.Parser as Parser exposing (Parser, s, top)
import Url exposing (Url)

type Route
    = Splash
    | Reader

parse: Url -> Route
parse url =
       let
           parser =
               Parser.oneOf
                   [ Parser.map Splash    ( top )
                   , Parser.map Reader    ( s "reader" )
                   ]
       in
           Maybe.withDefault Splash (Parser.parse parser url)
