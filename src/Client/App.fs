module App

open Global

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

open System
open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation
open Fulma
open Fulma.BulmaClasses
open Fable.PowerPack

type Message =
    | StorageFailure of exn
    | Logout
    | LoggedIn of UserData
    | LoggedOut
    | AuthTimedOut
    | LoginMsg of Login.Message
    | HomeMsg of Home.Message
    | WorkspaceMsg of Workspace.Message

type PageModel =
    | LoginPageModel of Login.Model
    | HomePageModel of Home.Model
    | WorkspacePageModel of Workspace.Model

type Model =
    { User : UserData option
      PageModel : PageModel }

let route : Parser<Page -> Page, Page> =
    oneOf
        [ map Login (s "login")
          map Home (s "home")
          map Workspace (s "workspaces" </> str) ]

let urlUpdate (result : Page option) model =
    match result with
    | Some Login ->
        let pm, pCmd = Login.init model.User
        { model with PageModel = LoginPageModel pm }, Cmd.map LoginMsg pCmd
    | Some Home ->
        let pm, pCmd = Home.init model.User
        { model with PageModel = HomePageModel pm }, Cmd.map HomeMsg pCmd
    | Some (Workspace wsId) ->
        match model.User with
        | Some u ->
            let pm, pCmd =
                Guid.Parse wsId
                |> Workspace.init u
            { model with PageModel = WorkspacePageModel pm }, Cmd.map WorkspaceMsg pCmd
        | None ->
            model, toHash Login |> Navigation.newUrl
    | None ->
        model, toHash Home |> Navigation.newUrl

let loadUser () : UserData option =
    BrowserLocalStorage.load "user"

let saveUserCmd (user : UserData) =
    Cmd.ofFunc (BrowserLocalStorage.save "user") user (fun _ -> LoggedIn user) StorageFailure

let deleteUserCmd =
    Cmd.ofFunc BrowserLocalStorage.delete "user" (fun _ -> LoggedOut) StorageFailure

let init result =
    let user = loadUser ()

    // the initial appload will initialize the model correctly
    let tmpPm = { Home.Model.State = Home.UnAuthenticated;
                  Home.Model.UserData = user;
                  Home.Model.Space = None
                  Home.Model.NewWorkspaceDialog = Home.NewWorkspaceDialog.empty () }

    let model =
        { User = user
          PageModel = HomePageModel tmpPm }

    urlUpdate result model

let update (msg : Message) (model : Model) =
    match msg, model.PageModel with
    | StorageFailure e, _ ->
        printfn "Local storage could not be accessed. %A" e
        model, Cmd.none
    | Logout, _ ->
        { model with User = None}, deleteUserCmd
    | LoggedOut, _ ->
        model, toHash Login |> Navigation.newUrl
    | LoggedIn user, _ ->
        { model with User = Some user }, toHash Home |> Navigation.newUrl
    | AuthTimedOut, _ ->
        { model with User = None }, Cmd.batch [ deleteUserCmd; toHash Login |> Navigation.newUrl ]
    | LoginMsg lim, LoginPageModel m ->
        let m, cmd, externalMsg = Login.update lim m

        let appCmd =
            match externalMsg with
            | Login.UserLoggedIn ud ->
                saveUserCmd ud
            | Login.NoOp -> Cmd.none

        { model with PageModel = LoginPageModel m}, Cmd.batch [ Cmd.map LoginMsg cmd; appCmd ]
    | LoginMsg _, _ -> model, Cmd.none
    | HomeMsg hm, HomePageModel pm ->
        let m, cmd, extMsg = Home.update hm pm

        let appCmd =
            match extMsg with
            | Home.AuthenticationError ->
                Cmd.ofMsg AuthTimedOut
            | Home.OpenWorkspace ws ->
                string ws.Id
                |> Workspace
                |> toHash
                |> Navigation.newUrl
            | Home.NoOp -> Cmd.none

        { model with PageModel = HomePageModel m}, Cmd.batch [ Cmd.map HomeMsg cmd; appCmd ]
    | HomeMsg _, _ -> model, Cmd.none
    | WorkspaceMsg wm, WorkspacePageModel pm ->
        let m, cmd, externalMsg = Workspace.update wm pm

        let appCmd =
            match externalMsg with
            | Workspace.AuthFailed ->
                Cmd.ofMsg AuthTimedOut
            | Workspace.NoOp -> Cmd.none

        { model with PageModel = WorkspacePageModel m }, Cmd.batch [ Cmd.map WorkspaceMsg cmd; appCmd ]
    | WorkspaceMsg _, _ -> model, Cmd.none


let viewPage (model : Model) dispatch =
    match model.PageModel with
    | LoginPageModel pm ->
        Login.view pm (LoginMsg >> dispatch)
    | HomePageModel pm ->
        Home.view pm (HomeMsg >> dispatch)
    | WorkspacePageModel pm ->
        Workspace.view pm (WorkspaceMsg >> dispatch)

let view (model : Model) dispatch =
    R.div [ RP.Class "main-wrapper" ]
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
          R.div [ RP.Class "page-content" ]
            [ viewPage model dispatch ]
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
// |> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run
