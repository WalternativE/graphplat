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

type Model =
    { UserData : UserData
      WorkspaceId : Guid
      Workspace : Workspace option
      PageState : PageState
      GraphModel : Graph.Model
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
    let gm, gcmd = Graph.init ()

    let qv = { IsActive = false }

    { UserData = userData
      WorkspaceId = wId
      Workspace = None
      PageState = FreshlyOpened
      GraphModel = gm
      QuickViewModel = qv }, Cmd.batch [ LookUpWorkspace wId |> Cmd.ofMsg; Cmd.map GraphMsg gcmd ]

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
        let m, cmd, ext = Graph.update gm model.GraphModel

        let appCmd =
            match ext with
            | Graph.NoOp -> Cmd.none
            | Graph.NodeSelected ->
                Cmd.ofMsg ShowQuickView

        { model with GraphModel = m }, Cmd.batch [ Cmd.map GraphMsg cmd; appCmd ], NoOp
    | HideQuickView ->
        { model with QuickViewModel = { model.QuickViewModel with IsActive = false } }, Cmd.none, NoOp
    | ShowQuickView ->
        { model with QuickViewModel = { model.QuickViewModel with IsActive = true } }, Cmd.none, NoOp

let viewWorkflowPane model dispatch =
    [ R.h1 [] [ R.str model.Workspace.Value.Name ]
      Container.container [ Container.IsFluid ]
        [ Graph.view model.GraphModel (GraphMsg >> dispatch) ]
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
        [ Container.container []
            (viewWorkspace model dispatch) ]
