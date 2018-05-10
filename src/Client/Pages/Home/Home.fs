module Home

open System

open Fulma
open Global
open Elmish

open Domain.Model

open Fable.Core.JsInterop

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type State =
    | Authenticated
    | UnAuthenticated

type DialogState =
    | Idle
    | Creating

type NewWorkspaceDialog =
    { DialogState : DialogState
      Name : string
      Id : Guid }

    static member empty () =
        { DialogState = Idle
          Name = ""
          Id = Guid.NewGuid () }

type Model =
    { State : State
      UserData : UserData option
      Space : UserSpace option
      NewWorkspaceDialog : NewWorkspaceDialog }

type ExternalMessage =
    | NoOp
    | AuthenticationError
    | OpenWorkspace of Workspace

type Message =
    | UserSpaceLoaded of UserSpace
    | FetchError of exn
    | NewWorkspaceClicked
    | CancelNewWorkspaceClicked
    | SetWorkspaceName of string
    | SubmitNewWorkspaceClicked
    | WorkSpaceCreated of Workspace
    | DeleteWorkspaceClicked of Workspace
    | WorkSapceDeleted of Workspace
    | OpenWorkspaceClicked of Workspace

let loadUserSpaceCmd token =
    Cmd.ofPromise ApiClient.loadUserSpace token UserSpaceLoaded FetchError

let createUserSpaceCmd token =
    Cmd.ofPromise ApiClient.createUserSpace token UserSpaceLoaded FetchError

let createWorkSpaceCmd (dialog : NewWorkspaceDialog) token =
    let ws =
        { Id = dialog.Id
          Name = dialog.Name
          Workflow = None }
    Cmd.ofPromise ApiClient.createWorkSpace (ws, token) WorkSpaceCreated FetchError

let deleteWorkSpaceCmd (workspace : Workspace ) token =
    Cmd.ofPromise ApiClient.deleteWorkspace (workspace.Id, token) (fun _ -> WorkSapceDeleted workspace ) FetchError

let init (user : UserData option) =
    let model = { State = UnAuthenticated;
                  UserData = user;
                  Space = None
                  NewWorkspaceDialog = NewWorkspaceDialog.empty () }

    if user.IsSome then
        {model with State = Authenticated }, loadUserSpaceCmd user.Value.Token
    else
        {model with State = UnAuthenticated}, Cmd.none

let update (msg : Message) (model : Model) =
    match msg with
    | UserSpaceLoaded us ->
        { model with Space = Some us}, Cmd.none, NoOp
    | FetchError e ->
        if e.Message = "404" then
            model, createUserSpaceCmd model.UserData.Value.Token, NoOp
        elif e.Message = "400" then
            printfn "Cient error happend"
            model, Cmd.none, NoOp
        elif e.Message = "401" then
            printfn "Request could not be authenticated"
            model, Cmd.none, AuthenticationError
        else
            model, Cmd.none, NoOp
    | NewWorkspaceClicked ->
        { model with NewWorkspaceDialog = { model.NewWorkspaceDialog with DialogState = Creating } }, Cmd.none, NoOp
    | CancelNewWorkspaceClicked ->
        { model with NewWorkspaceDialog = NewWorkspaceDialog.empty () }, Cmd.none, NoOp
    | SetWorkspaceName n ->
        { model with NewWorkspaceDialog = { model.NewWorkspaceDialog with Name = n } }, Cmd.none, NoOp
    | SubmitNewWorkspaceClicked ->
        model, createWorkSpaceCmd model.NewWorkspaceDialog model.UserData.Value.Token ,NoOp
    | WorkSpaceCreated ws ->
        let space =
            model.Space
            |> Option.map (fun s -> { s with Workspaces = ws::s.Workspaces})
        { model with Space = space; NewWorkspaceDialog = NewWorkspaceDialog.empty () }, Cmd.none, NoOp
    | DeleteWorkspaceClicked ws ->
        model, deleteWorkSpaceCmd ws model.UserData.Value.Token, NoOp
    | WorkSapceDeleted ws ->
        let space =
            model.Space
            |> Option.map (fun s ->
                    let wss =
                        s.Workspaces
                        |> List.filter (fun w -> w = ws |> not)
                    { s with Workspaces = wss})
        { model with Space = space }, Cmd.none, NoOp
    | OpenWorkspaceClicked ws ->
        model, Cmd.none, OpenWorkspace ws

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
                    [ Card.header [] [ Card.Header.title [] [ R.str "Workspace" ] ]
                      Card.content [ GenericOption.CustomClass "card-content--with-min-height" ]
                        [ sprintf "Id: %A" ws.Id |> R.str
                          R.br []
                          sprintf "Name: %s" ws.Name |> R.str ]
                      Card.footer []
                        [ Card.Footer.item [ GenericOption.Props [ RP.OnClick (fun _ -> OpenWorkspaceClicked ws |> dispatch) ] ] [ R.str "Open" ]
                          Card.Footer.item [ GenericOption.Props [ RP.OnClick (fun _ -> DeleteWorkspaceClicked ws |> dispatch) ] ] [ R.str "Delete" ] ] ] ] ]
    | None -> []

let viewNewWorkspaceDialog (model : Model) dispatch =
    let dynamicPart =
        match model.NewWorkspaceDialog.DialogState with
        | Idle ->
            [ Card.content [ GenericOption.CustomClass "card-content--with-min-height" ]
                [ workspaceText ]
              Card.footer []
                [ Card.Footer.item [ GenericOption.Props [ RP.OnClick (fun _ -> dispatch NewWorkspaceClicked) ] ] [ R.str "New" ] ] ]
        | Creating ->
            [ Card.content [ GenericOption.CustomClass "card-content--with-min-height" ]
                [ R.form []
                    [ Field.div []
                        [ Label.label [] [ R.str "Workspace Name"]
                          Control.div []
                            [ Input.text
                                [ Input.OnChange (fun ev -> dispatch (SetWorkspaceName !!ev.target?value) ) ] ] ] ] ]
              Card.footer []
                [ Card.Footer.item
                    [ GenericOption.Props [ RP.OnClick (fun _ -> dispatch SubmitNewWorkspaceClicked) ] ] [ R.str "Submit" ]
                  Card.Footer.item
                    [ GenericOption.Props [ RP.OnClick (fun _ -> dispatch CancelNewWorkspaceClicked) ] ] [ R.str "Cancel" ] ] ]

    Column.column [ oneThirdDesktop ]
        [ Card.card [  GenericOption.CustomClass "card--with-min-height" ]
            [ yield Card.header [] [ Card.Header.title [] [ R.str "New Workspace" ] ]
              yield! dynamicPart ] ]

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
              yield (viewNewWorkspaceDialog model dispatch) ] ]

let view model dispatch =
    Section.section []
        [ Container.container [ Container.IsFluid ]
            (viewContent model dispatch) ]
