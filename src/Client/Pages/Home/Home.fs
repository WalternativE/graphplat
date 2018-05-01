module Home

open Fulma
open Global
open Elmish

open Domain.Model
open Fable.PowerPack
open Fable.PowerPack.Fetch.Fetch_types

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type State =
    | Authenticated
    | UnAuthenticated

type Model =
    { State : State
      UserData : UserData option
      Space : UserSpace option }

type Message =
    | UserSpaceLoaded of UserSpace
    | FetchError of exn

let loadUserSpace (token : JWT) = promise {
    let props =
        [ RequestProperties.Method HttpMethod.GET
          Fetch.requestHeaders
            [ HttpRequestHeaders.ContentType "application/json"
              HttpRequestHeaders.Authorization ("Bearer " + token) ] ]

    try
        return! Fetch.fetchAs<UserSpace> "/api/secured/users/current/user-space" props
    with e ->
        return
            e.Message
            |> extractFetchError
            |> failwithf "%s"
}

let loadUserSpaceCmd token =
    Cmd.ofPromise loadUserSpace token UserSpaceLoaded FetchError

let createUserSpace (token : JWT) = promise {
    let props =
        [ RequestProperties.Method HttpMethod.POST
          Fetch.requestHeaders
            [ HttpRequestHeaders.ContentType "application/json"
              HttpRequestHeaders.Authorization ("Bearer " + token) ] ]

    try
        return! Fetch.fetchAs<UserSpace> "/api/secured/users/current/user-space" props
    with e ->
        return
            e.Message
            |> extractFetchError
            |> failwithf "%s"
}

let createUserSpaceCmd token =
    Cmd.ofPromise createUserSpace token UserSpaceLoaded FetchError

let init (user : UserData option) =
    let model = { State = UnAuthenticated; UserData = user; Space = None }

    if user.IsSome then
        {model with State = Authenticated }, loadUserSpaceCmd user.Value.Token
    else
        {model with State = UnAuthenticated}, Cmd.none

let update (msg : Message) (model : Model) =
    match msg with
    | UserSpaceLoaded us ->
        { model with Space = Some us}, Cmd.none
    | FetchError e ->
        if e.Message = "404" then
            model, createUserSpaceCmd model.UserData.Value.Token
        elif e.Message = "400" then
            printfn "User space is already created!"
            model, Cmd.none
        else
            model, Cmd.none

let oneThirdDesktop =
    Column.Width (Column.Desktop, Column.IsOneThird)

let workspaceText =
    Content.content []
        [ R.str "Workspaces are containers for your analytics pipelines."
          R.br []
          R.str "Just create a new workspace and start working." ]

let viewWorkspaces (model : Model) dispatch =
    match model.Space with
    | Some us ->
        [ for ws in us.Workspaces do
            yield Column.column [ oneThirdDesktop ]
                [ Card.card []
                    [ Card.header [] [ Card.Header.title [] [ R.str "Bla" ] ]
                      Card.content [ GenericOption.CustomClass "card-content--with-min-height" ]
                        [ R.str "bla" ]
                      Card.footer []
                        [ Card.Footer.item [] [ R.str "Edit" ]
                          Card.Footer.item [] [ R.str "Open" ] ] ] ] ]
    | None -> []

let viewContent (model : Model) dispatch =
    match model.State with
    | UnAuthenticated ->
        [ R.h1 [ RP.Class "is-size-3" ] [ R.str "Weclome, please login to proceed!" ] ]
    | Authenticated ->
        [ R.h1 [ RP.Class "is-size-3" ] [ sprintf "Welcome, %s" model.UserData.Value.User.Name |> R.str ]
          R.hr []
          Columns.columns [ Columns.IsMultiline ]
            [ yield Column.column [ oneThirdDesktop ]
                [ Card.card []
                    [ Card.header [] [ Card.Header.title [] [ R.str "User Settings" ] ]
                      Card.content [ GenericOption.CustomClass "card-content--with-min-height"  ]
                        [ R.str "Coming soon-ish" ]
                      Card.footer []
                        [ Card.Footer.item [] [ R.str "Edit" ] ] ] ]
              yield! viewWorkspaces model dispatch
              yield Column.column [ oneThirdDesktop ]
                [ Card.card [  GenericOption.CustomClass "card--with-min-height" ]
                    [ Card.header [] [ Card.Header.title [] [ R.str "New Workspace" ] ]
                      Card.content [ GenericOption.CustomClass "card-content--with-min-height" ]
                        [ workspaceText ]
                      Card.footer []
                        [ Card.Footer.item [] [ R.str "New" ] ] ] ] ] ]

let view model dispatch =
    Section.section []
        [ Container.container [ Container.IsFluid ]
            (viewContent model dispatch) ]
