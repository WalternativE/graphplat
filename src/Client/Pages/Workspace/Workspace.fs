module Workspace

open Global
open Domain.Model
open Domain.Commands

open System
open Fulma
open Fulma.Extensions
open Elmish
open ApiClient

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props
module B = Fable.Import.Browser

type PageState =
    | FreshlyOpened
    | Initialised
    | InitialisationFailed

type QuickViewModel =
    { IsActive : bool }

type Model =
    { UserData : UserData
      WorkspaceId : Guid
      Workspace : Workspace option
      PageState : PageState
      GraphModel : Graph.Model option
      QuickViewModel : QuickViewModel }

type ExternalMessage =
    | NoOp
    | AuthFailed

type Message =
    | LookUpWorkspace of Guid
    | WorkspaceFound of Workspace
    | WorkflowCreated of Workflow
    | FetchError of exn
    | GraphMsg of Graph.Msg
    | HideQuickView
    | ShowQuickView
    | AddStep of AddStep
    | StepAdded of Workflow

let init (userData : UserData) (wId : Guid) =
    let qv = { IsActive = false }

    { UserData = userData
      WorkspaceId = wId
      Workspace = None
      PageState = FreshlyOpened
      GraphModel = None
      QuickViewModel = qv }, LookUpWorkspace wId |> Cmd.ofMsg

let getWorkspaceCmd wsId token =
    Cmd.ofPromise getWorkspace (wsId, token) WorkspaceFound FetchError

let createWorkflowCmd workflow token =
    Cmd.ofPromise createWorkflow (workflow, token) WorkflowCreated FetchError

let addStepCmd workflow addStep token =
    Cmd.ofPromise addWorkflowStep ((workflow, addStep), token) StepAdded FetchError

let update (msg : Message) (model : Model) =
    match msg with
    | LookUpWorkspace wsid ->
        model, getWorkspaceCmd wsid model.UserData.Token, NoOp
    | WorkspaceFound ws ->
        let (graphModel, cmd) =
            match ws.Workflow with
            | Some wf ->
                let gm, gcmd = Graph.init wf.WorkflowTree
                (Some gm, gcmd |> Cmd.map GraphMsg)
            | None ->
                let newWf =
                    { Id = Guid.NewGuid ()
                      AssignedWorkspace = ws.Id
                      WorkflowTree = Empty }
                None, createWorkflowCmd newWf model.UserData.Token

        { model with
            PageState = Initialised
            Workspace = Some ws
            GraphModel = graphModel }, cmd, NoOp
    | WorkflowCreated wf ->
        let ws =
            model.Workspace
            |> Option.map (fun ws -> { ws with Workflow = Some wf })
        { model with Workspace = ws }, Cmd.none, NoOp
    | FetchError e ->
        if e.Message = "401" then
            model, Cmd.none, AuthFailed
        elif e.Message = "409" then
            B.console.log "A conflict happened creating the new workflow"
            { model with PageState = InitialisationFailed }, Cmd.none, NoOp
        else
            { model with PageState = InitialisationFailed }, Cmd.none, NoOp
    | GraphMsg gm ->
        match model.GraphModel with
        | Some graphModel ->
            let m, cmd, ext = Graph.update gm graphModel

            let appCmd =
                match ext with
                | Graph.NoOp -> Cmd.none
                | Graph.NodeSelected ->
                    Cmd.ofMsg ShowQuickView
                | Graph.AddStep addStep ->
                    Cmd.ofMsg (AddStep addStep)

            { model with GraphModel = Some m }, Cmd.batch [ Cmd.map GraphMsg cmd; appCmd ], NoOp
        | None -> model, Cmd.none, NoOp
    | HideQuickView ->
        { model with QuickViewModel = { model.QuickViewModel with IsActive = false } }, Cmd.none, NoOp
    | ShowQuickView ->
        { model with QuickViewModel = { model.QuickViewModel with IsActive = true } }, Cmd.none, NoOp
    | AddStep addStep ->
        match model.Workspace with
        | None -> model, Cmd.none, NoOp
        | Some ws ->
            match ws.Workflow with
            | None -> model, Cmd.none, NoOp
            | Some wf -> model, addStepCmd wf addStep model.UserData.Token, NoOp
    | StepAdded wf ->
        // TODO the graph component keeps its changes in memory - I might want to sync here
        { model with
            Workspace =
                model.Workspace
                |> Option.map (fun ws -> { ws with Workflow = Some wf }) }, Cmd.none, NoOp

let viewGraphPanel model dispatch =
    match model.GraphModel with
    | None ->
        [ Content.content []
            [ R.str "Loading..." ] ]
    | Some gm ->
        [ Content.content []
            [ Graph.view gm (GraphMsg >> dispatch) ] ]

let viewWorkflowPane model dispatch =
    [ R.h1 [ RP.Class "is-size-3" ] [ R.str model.Workspace.Value.Name ]
      R.hr []
      Container.container [ Container.IsFluid ]
        (viewGraphPanel model dispatch)
      Quickview.quickview [ Quickview.IsActive model.QuickViewModel.IsActive ]
        [ Quickview.header []
            [ Quickview.title [] [ R.str "Testing" ]
              Delete.delete [ Delete.OnClick (fun _ -> dispatch HideQuickView) ] [] ]
          Quickview.body [] [ R.str "Blub..." ]
          Quickview.footer []
            [ Button.button [ Button.OnClick (fun _ -> dispatch HideQuickView) ]
                [ R.str "Close" ] ] ] ]

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
        [ Container.container [ Container.IsFluid ]
            (viewWorkspace model dispatch) ]
