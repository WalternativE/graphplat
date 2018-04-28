module App

open Global

open Fable
module R = Fable.Helpers.React

open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation

let route : Parser<Page -> Page, Page> =
    oneOf
        [ map Login (s "login")
          map Home (s "home") ]

let urlUpdate (result : Page option) model =
    match result with
    | Some Login ->
        { model with Page = result.Value }, Cmd.none
    | Some Home ->
        { model with Page = result.Value }, Cmd.none
    | None ->
        model, Navigation.modifyUrl "#/"

let init result =
    let model =
        { User = None
          Page = Login }

    urlUpdate result model

let update (msg : Message) (model : Model) =
    match msg with
    | ChangePage route -> { model with Page = route }, Cmd.none

let view (model : Model) dispatch =
    let pageHtml =
        match model.Page with
        | Login -> Login.view
        | Home -> Home.view

    pageHtml

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable (parseHash route) urlUpdate
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
