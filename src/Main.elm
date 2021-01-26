module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Html
import Url exposing (Url)

import Page.Splash as Splash
import Page.Reader as Reader
import App.Router as Router

main = Browser.application
       { init = init
       , update = update
       , view = view
       , subscriptions = subs
       , onUrlRequest = LinkClicked
       , onUrlChange = UrlChanged
       }

type Model
    = Splash Splash.Model
    | Reader Reader.Model

init: () -> Url -> Nav.Key -> (Model, Cmd msg)
init _ url key =
    (applyRoute url key)

subs: Model -> Sub Msg
subs model =
    case model of
        Reader rm ->
            Reader.subscriptions ReaderMsg rm
        _ ->
            Sub.none

-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SplashMsg Splash.Msg
    | ReaderMsg Reader.Msg

getAppKey: Model -> Nav.Key
getAppKey model =
    case model of
        Splash sm ->
            sm.appKey
        Reader rm ->
            rm.appKey

applyRoute url key =
    case (Router.parse url) of
        Router.Splash ->
            ( (Splash <| Splash.init key)
            , Cmd.none
            )
        Router.Reader ->
            ( (Reader <| Reader.init key)
            , Cmd.none
            )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg,model) of

        (SplashMsg smsg, Splash smod) ->
            Splash.update Splash smsg smod

        (ReaderMsg rmsg, Reader rmod) ->
            Reader.update Reader rmsg rmod

        (LinkClicked urlReq, _) ->
            case urlReq of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (getAppKey model) (Url.toString url)
                    )
                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        (UrlChanged url, _) ->
            let
                key = getAppKey model
            in
            (applyRoute url key)

        (_, _) ->
            ( model
            , Cmd.none
            )

-- VIEW

view: Model -> Browser.Document Msg
view model =
    let
        makeHtml topEl =
            topEl |> Element.layout []

        makeDoc toMsg topEl =
            { title = "Hoppr v2.0"
            , body =
                  [ Html.map toMsg (makeHtml topEl) ]
            }
    in
    case model of
        Splash smod ->
            makeDoc SplashMsg (Splash.view smod)
        Reader rmod ->
            makeDoc ReaderMsg (Reader.view rmod)
