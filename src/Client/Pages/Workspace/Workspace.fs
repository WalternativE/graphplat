module Workspace

open Global
open Domain.Model

open System
open Fulma
open Fulma.Extensions
open Elmish
open ApiClient

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props

type PageState =
    | FreshlyOpened
    | Initialised
    | InitialisationFailed

type QuickViewModel =
    { IsActive : bool }

type GraphDisplay =
    | NotInitialized
    | NoWorkflowExisting
    | ModelFound of Workflow * Graph.Model

type Model =
    { UserData : UserData
      WorkspaceId : Guid
      Workspace : Workspace option
      PageState : PageState
      GraphDisplay : GraphDisplay
      QuickViewModel : QuickViewModel }

type ExternalMessage =
    | NoOp
    | AuthFailed

type Message =
    | LookUpWorkspace of Guid
    | WorkspaceFound of Workspace
    | FetchError of exn
    | GraphMsg of Graph.Msg
    | HideQuickView
    | ShowQuickView

let init (userData : UserData) (wId : Guid) =
    let qv = { IsActive = false }

    { UserData = userData
      WorkspaceId = wId
      Workspace = None
      PageState = FreshlyOpened
      GraphDisplay = NotInitialized
      QuickViewModel = qv }, LookUpWorkspace wId |> Cmd.ofMsg

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
        match model.GraphDisplay with
        | ModelFound (wf, graphModel) ->
            let m, cmd, ext = Graph.update gm graphModel

            let appCmd =
                match ext with
                | Graph.NoOp -> Cmd.none
                | Graph.NodeSelected ->
                    Cmd.ofMsg ShowQuickView

            { model with GraphDisplay = ModelFound (wf, m) }, Cmd.batch [ Cmd.map GraphMsg cmd; appCmd ], NoOp
        | _ -> model, Cmd.none, NoOp
    | HideQuickView ->
        { model with QuickViewModel = { model.QuickViewModel with IsActive = false } }, Cmd.none, NoOp
    | ShowQuickView ->
        { model with QuickViewModel = { model.QuickViewModel with IsActive = true } }, Cmd.none, NoOp

let viewGraphPanel model dispatch =
    match model.GraphDisplay with
    | NotInitialized ->
        [ Content.content []
            [ R.str "Loading..." ] ]
    | GraphDisplay.NoWorkflowExisting ->
        [ Content.content []
            [ R.str "Cannot find a workflow assigned to the workspace." ] ]
    | GraphDisplay.ModelFound (_, gm) ->
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
