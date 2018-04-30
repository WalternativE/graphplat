module Home

open Fulma
open Global
open Elmish

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type State =
    | Authenticated
    | UnAuthenticated

type Model =
    { State : State
      User : UserData option }

let init (user : UserData option) =
    let state = if user.IsSome then Authenticated else UnAuthenticated

    { State = state; User = user }, Cmd.none

let oneThirdDesktop =
    Column.Width (Column.Desktop, Column.IsOneThird)

let workspaceText =
    Content.content []
        [ R.str "Workspaces are containers for your analytics pipelines."
          R.br []
          R.str "Just create a new workspace and start working." ]

let viewContent (model : Model) dispatch =
    match model.State with
    | UnAuthenticated ->
        [ R.h1 [ RP.Class "is-size-3" ] [ R.str "Weclome, please login to proceed!" ] ]
    | Authenticated ->
        [ R.h1 [ RP.Class "is-size-3" ] [ sprintf "Welcome, %s" model.User.Value.UserName |> R.str ]
          R.hr []
          Columns.columns [ Columns.IsMultiline ]
            [ Column.column [ oneThirdDesktop ]
                [ Card.card []
                    [ Card.header [] [ Card.Header.title [] [ R.str "User Settings" ] ]
                      Card.content [ GenericOption.CustomClass "card-content--with-min-height"  ]
                        [ R.str "Coming soon-ish" ]
                      Card.footer []
                        [ Card.Footer.item [] [ R.str "Edit" ] ] ] ]
              Column.column [ oneThirdDesktop ]
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
