module App

open Global

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation
open Fulma
open Fulma.BulmaClasses
open Fable.PowerPack

let route : Parser<Page -> Page, Page> =
    oneOf
        [ map Login (s "login")
          map Home (s "home") ]

let urlUpdate (result : Page option) model =
    match result with
    | Some Login ->
        { model with PageModel = LoginPageModel }, Cmd.none
    | Some Home ->
        { model with PageModel = HomePageModel }, Cmd.none
    | None ->
        model, Navigation.modifyUrl "#/"

let loadUser () : UserData option =
    BrowserLocalStorage.load "user"

let saveUserCmd (user : UserData) =
    Cmd.ofFunc (BrowserLocalStorage.save "user") user (fun _ -> LoggedIn user) StorageFailure

let deleteUserCmd =
    Cmd.ofFunc BrowserLocalStorage.delete "user" (fun _ -> LoggedOut) StorageFailure

let init result =
    let user = loadUser ()

    let model =
        { User = user
          PageModel = LoginPageModel }

    urlUpdate result model

let update (msg : Message) (model : Model) =
    match msg with
    | StorageFailure e ->
        printfn "Local storage could not be accessed. %A" e
        model, Cmd.none
    | Logout ->
        { model with User = None}, deleteUserCmd
    | LoggedOut ->
        model, toHash Login |> Navigation.modifyUrl
    | LoggedIn user ->
        { model with User = Some user }, toHash Home |> Navigation.modifyUrl

let viewPage (model : Model) dispatch =
    match model.PageModel with
    | LoginPageModel -> Login.view
    | HomePageModel -> Home.view

let view (model : Model) dispatch =
    R.div []
        [ Navbar.navbar [ Navbar.IsFixedTop ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ RP.Href "#/" ] ]
                    [ R.str "GraphPlat" ] ]
              Navbar.Item.a [ Navbar.Item.Props [ toHash Home |> RP.Href ] ]
                [ R.str "Home" ]
              Navbar.End.div []
                [ Navbar.Item.div []
                    [ if model.User.IsSome then
                        yield Button.button [ Button.Color IsPrimary; Button.OnClick (fun _ -> dispatch Logout) ] [ R.str "Logout" ]
                      else
                        yield Button.a [ Button.Color IsPrimary; Button.Props [ toHash Login |> RP.Href ] ] [ R.str "Login"] ] ] ]
          R.hr []
          viewPage model dispatch
          Footer.footer []
            [ Content.content [ Content.CustomClass Bulma.Properties.Alignment.HasTextCentered ]
                [ R.h6 [] [ R.str "GraphPlat Dev - Copyright Gregor Beyerle" ] ] ] ]

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
