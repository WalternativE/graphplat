module Workspace

open Global
open Domain.Model

open System
open Fulma
open Elmish
open ApiClient

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type PageState =
    | FreshlyOpened
    | Initialised
    | InitialisationFailed

type Model =
    { UserData : UserData
      WorkspaceId : Guid
      Workspace : Workspace option
      PageState : PageState
      GraphModel : Graph.Model }

type ExternalMessage =
    | NoOp
    | AuthFailed

type Message =
    | LookUpWorkspace of Guid
    | WorkspaceFound of Workspace
    | FetchError of exn
    | GraphMsg of Graph.Msg

let init (userData : UserData) (wId : Guid) =
    let gm, gcmd = Graph.init ()

    { UserData = userData
      WorkspaceId = wId
      Workspace = None
      PageState = FreshlyOpened
      GraphModel = gm }, Cmd.batch [ LookUpWorkspace wId |> Cmd.ofMsg; Cmd.map GraphMsg gcmd ]

let getWorkspaceCmd wsId token =
    Cmd.ofPromise getWorkspace (wsId, token) WorkspaceFound FetchError

let update (msg : Message) (model : Model) =
    match msg with
    | LookUpWorkspace wsid ->
        model, getWorkspaceCmd wsid model.UserData.Token, NoOp
    | WorkspaceFound ws ->
        { model with PageState = Initialised; Workspace = Some ws}, Cmd.none, NoOp
    | FetchError e ->
        if e.Message = "401" then
            model, Cmd.none, AuthFailed
        else
            { model with PageState = InitialisationFailed }, Cmd.none, NoOp
    | GraphMsg gm ->
        let m, cmd = Graph.update gm model.GraphModel
        { model with GraphModel = m }, Cmd.map GraphMsg cmd, NoOp

let viewWorkflowPane model dispatch =
    [ R.h1 [] [ R.str model.Workspace.Value.Name ]
      Container.container []
        [ Graph.view model.GraphModel (GraphMsg >> dispatch) ] ]

let viewWorkspace model dispatch =
    match model.PageState with
    | FreshlyOpened ->
        [ R.h1 [] [ R.str "Loading..." ] ]
    | InitialisationFailed ->
        [ R.h1 [] [ R.str "The requested workspace does not exist :("] ]
    | Initialised ->
        (viewWorkflowPane model dispatch)

let view model dispatch =
    Section.section []
        [ Container.container []
            (viewWorkspace model dispatch) ]
