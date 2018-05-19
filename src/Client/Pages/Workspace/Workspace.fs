module Workspace

open System

open Fable.Core.JsInterop
open Elmish

open Fulma
open Fulma.FontAwesome
open Fulma.Extensions

open Domain
open Domain.Model
open Domain.Commands

open Global
open ApiClient
open Shared

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props
module B = Fable.Import.Browser
module C = Cytoscape

type PageState =
    | FreshlyOpened
    | Initialised
    | InitialisationFailed

type GraphModalModel =
    { IsActive : bool
      GraphToDisplay : SimpleGraph option }

type QuickViewModel =
    { IsActive : bool
      Step : WorkflowStep option
      EditStepState : WorkflowStep option }

type Model =
    { UserData : UserData
      WorkspaceId : Guid
      Workspace : Workspace option
      PageState : PageState
      GraphModel : Graph.Model option
      QuickViewModel : QuickViewModel
      LastWorkflowOutput : LabeledOutput list option
      GraphModalModel : GraphModalModel }

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
    | ShowQuickView of WorkflowStep option
    | AddStep of AddStep
    | StepAdded of Workflow
    | StageStepEdit of WorkflowStep
    | ResetStepEdits
    | SaveWorkflowStepClicked of WorkflowStep
    | WorkflowStepChanged of Workflow
    | ExecuteWorkflow of WorkflowId
    | WorkflowExecuted of LabeledOutput list
    | OpenGraphModal of SimpleGraph
    | CloseGraphModal

let init (userData : UserData) (wId : Guid) =
    let gm = { IsActive = false; GraphToDisplay = None }
    let qv = { IsActive = false; Step = None; EditStepState = None }

    { UserData = userData
      WorkspaceId = wId
      Workspace = None
      PageState = FreshlyOpened
      GraphModel = None
      QuickViewModel = qv
      LastWorkflowOutput = None
      GraphModalModel = gm }, LookUpWorkspace wId |> Cmd.ofMsg

let getWorkspaceCmd wsId token =
    Cmd.ofPromise getWorkspace (wsId, token) WorkspaceFound FetchError

let createWorkflowCmd workflow token =
    Cmd.ofPromise createWorkflow (workflow, token) WorkflowCreated FetchError

let addStepCmd workflow addStep token =
    Cmd.ofPromise addWorkflowStep ((workflow, addStep), token) StepAdded FetchError

let changeStepCmd step token =
    Cmd.ofPromise changeWorkflowStep (step, token) WorkflowStepChanged FetchError

let executeWorkflowCmd wfId token =
    Cmd.ofPromise executeWorkflow (wfId, token) WorkflowExecuted FetchError

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
        let gm, gcmd = Graph.init wf.WorkflowTree

        let ws =
            model.Workspace
            |> Option.map (fun ws -> { ws with Workflow = Some wf })
        { model with Workspace = ws; GraphModel = Some gm }, Cmd.map GraphMsg gcmd, NoOp
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
                | Graph.NodeSelected wfsId ->
                    let step =
                        graphModel.Workflow
                        |> Behaviour.tryFindWorkflowStep (fun step -> step.Id = wfsId)
                    B.console.log wfsId
                    ShowQuickView step |> Cmd.ofMsg
                | Graph.AddStep addStep ->
                    Cmd.ofMsg (AddStep addStep)

            { model with GraphModel = Some m }, Cmd.batch [ Cmd.map GraphMsg cmd; appCmd ], NoOp
        | None -> model, Cmd.none, NoOp
    | HideQuickView ->
        { model with
            QuickViewModel =
                { model.QuickViewModel with
                    IsActive = false; Step = None; EditStepState = None } }, Cmd.none, NoOp
    | ShowQuickView step->
        { model with
            QuickViewModel =
                { model.QuickViewModel with
                    IsActive = true; Step = step; EditStepState = step } }, Cmd.none, NoOp
    | AddStep addStep ->
        match model.Workspace with
        | None -> model, Cmd.none, NoOp
        | Some ws ->
            match ws.Workflow with
            | None -> model, Cmd.none, NoOp
            | Some wf -> model, addStepCmd wf addStep model.UserData.Token, NoOp
    | StepAdded wf ->
        let msg =
            Graph.ForceSync wf.WorkflowTree
            |> GraphMsg
            |> Cmd.ofMsg

        { model with
            Workspace =
                model.Workspace
                |> Option.map (fun ws -> { ws with Workflow = Some wf })}, msg, NoOp
    | StageStepEdit step ->
        { model with
            QuickViewModel =
                { model.QuickViewModel with
                    EditStepState = Some step}}, Cmd.none, NoOp
    | ResetStepEdits ->
        { model with
            QuickViewModel =
                { model.QuickViewModel with
                    EditStepState = model.QuickViewModel.Step }}, Cmd.none, NoOp
    | SaveWorkflowStepClicked step ->
        model, changeStepCmd step model.UserData.Token, NoOp
    | WorkflowStepChanged wf ->
        let msg =
            Graph.ForceSync wf.WorkflowTree
            |> GraphMsg
            |> Cmd.ofMsg

        let ws =
            model.Workspace
            |> Option.map (fun ws -> { ws with Workflow = Some wf})
        { model with Workspace = ws}, Cmd.batch [ msg; Cmd.ofMsg HideQuickView ], NoOp
    | ExecuteWorkflow wfId ->
        model, executeWorkflowCmd wfId model.UserData.Token, NoOp
    | WorkflowExecuted wfOutputs ->
        { model with LastWorkflowOutput = Some wfOutputs }, Cmd.none, NoOp
    | OpenGraphModal sg ->
        let nodes =
            sg.Nodes
            |> List.map (fun n -> string n.Id)
            |> Set.ofList

        let edges =
            sg.Edges
            |> List.map (fun e -> string e.From, string e.To)

        let graph =
            C.Utilities.createGraph nodes edges

        let el = B.document.getElementById "graph-modal-cy-container"
        let opts = createEmpty<C.Cytoscape.CytoscapeOptions>
        opts.container <- (Some el)
        opts.elements <- Some !^graph

        C.cytoscape opts |> ignore

        { model with GraphModalModel = { model.GraphModalModel with IsActive = true } }, Cmd.none, NoOp
    | CloseGraphModal ->
        { model with GraphModalModel = { model.GraphModalModel with IsActive = false } }, Cmd.none, NoOp


let viewGraphPanel model dispatch =
    match model.GraphModel with
    | None ->
        [ Content.content []
            [ R.str "Loading..." ] ]
    | Some gm ->
        [ Button.button
            [ Button.Color IsPrimary
              Button.OnClick (fun _ ->
                model.Workspace
                |> Option.bind (fun ws -> ws.Workflow)
                |> Option.iter (fun wf -> ExecuteWorkflow wf.Id |> dispatch) ) ]
            [ Icon.faIcon [] [ Fa.icon Fa.I.Play ]
              R.span [] [ R.str "Execute" ] ]
          Content.content []
            [ Graph.view gm (GraphMsg >> dispatch) ] ]

let labelFromStepType (st : StepType) =
    match st with
    | Unassigned -> "Unassigned"
    | InputStep _ -> "Input"
    | OutputStep _ -> "Output"
    | ComputationStep -> "Computation"

let stepTypeFromLabel (l : string) =
    match l with
    | "Input" -> EmptyInput |> InputStep
    | "Output" -> OutputStep ""
    | "Computation" -> ComputationStep
    | _ -> Unassigned

let viewStepTypeOptions (step : WorkflowStep) dispatch =
    match step.StepType with
    | Unassigned -> []
    | InputStep inputType ->
        // TODO when I'm working on input repositories I have to make this do something
        match inputType with
        | InputType.EmptyInput -> [ Field.div [] [ R.str "Empty Input" ] ]
        | InputType.ConstantInput -> [ Field.div [] [R.str "Constant Input"] ]
    | OutputStep label ->
        [ Field.div []
            [ Label.label [] [ R.str "Output Label" ]
              Input.text
                [ Input.Value label
                  Input.OnChange (fun ev ->
                    let output = ev.target?value |> string |> OutputStep
                    StageStepEdit { step with StepType = output } |> dispatch) ] ] ]
    | ComputationStep -> []

let handleAddNewStep (step : WorkflowStep) (dispatch : Message -> unit) =
    Behaviour.newWorkflowStep ()
    |> (fun sta -> AddStepAfter (step, sta))
    |> (fun adds ->  Message.AddStep adds )
    |> dispatch

let viewAddNode step dispatch =
    match step.StepType with
    | Unassigned -> []
    | OutputStep _ -> []
    | _ ->
        [ Field.div []
            [ Control.div []
                [ Button.button [ Button.OnClick (fun _ -> handleAddNewStep step dispatch) ]
                    [R.str "Attach new step" ] ] ] ]

let viewLastOutputDetails step model dispatch =
    let unpackOutput (output : LabeledOutput) =
        match output.Output with
        | Unit -> R.str "Empty output"
        | Scalar v -> sprintf "Scalar output of %f" v |> R.str
        | Error m -> sprintf "Error output with message: %s" m |> R.str
        | Graph g ->
            Button.button [ Button.OnClick (fun _ -> OpenGraphModal g |> dispatch ) ]
             [ sprintf "Graph Output" |> R.str ]

    let fromOutputs (label : OutputLabel) (outputs : LabeledOutput list) =
        outputs
        |> List.filter (fun o -> o.Label = label )
        |> List.map (fun o ->
                R.div []
                    [ unpackOutput o ] )

    match model.LastWorkflowOutput with
    | None -> []
    | Some wfOutputs ->
        match step.StepType with
        | Unassigned -> []
        | InputStep _ -> []
        | ComputationStep -> []
        | OutputStep label ->
            [ yield R.hr []
              yield! fromOutputs label wfOutputs ]

let viewQuickViewBody model dispatch =
    match model.QuickViewModel.Step, model.QuickViewModel.EditStepState with
    | Some baseStep, Some editStep ->
        Quickview.body []
            [ Content.content [ Content.CustomClass "quickview-content--with-padding" ]
                [ R.str <| sprintf "Editing step %O" editStep.Id
                  R.br []
                  R.form []
                    [ yield Field.div []
                        [ Label.label [] [ R.str "Type" ]
                          Select.select []
                            [ R.select
                                [ RP.Value (labelFromStepType editStep.StepType)
                                  RP.OnChange (fun ev ->
                                      let newType = ev.target?value |> string |> stepTypeFromLabel
                                      StageStepEdit { editStep with StepType = newType} |> dispatch ) ]
                                [ R.option [ RP.Value (labelFromStepType Unassigned) ] [ R.str "Unassigned" ]
                                  R.option [ RP.Value (InputStep EmptyInput |> labelFromStepType) ] [ R.str "Input" ]
                                  R.option [ RP.Value (OutputStep "" |> labelFromStepType) ] [ R.str "Ouput" ]
                                  R.option [ RP.Value (labelFromStepType ComputationStep) ] [R.str "Computation" ] ] ] ]
                      yield! viewStepTypeOptions editStep dispatch
                      yield Field.div [ Field.IsGrouped ]
                        [ Control.div []
                            [ Button.button
                                [ Button.Disabled (baseStep = editStep)
                                  Button.Color Color.IsPrimary
                                  Button.OnClick (fun _ -> SaveWorkflowStepClicked editStep |> dispatch) ]
                                [ R.str "Save" ] ]
                          Control.div []
                            [ Button.button
                                [ Button.Disabled (baseStep = editStep)
                                  Button.OnClick (fun _ -> dispatch ResetStepEdits) ]
                                [ R.str "Cancel" ] ] ]
                      yield! viewAddNode baseStep dispatch
                      yield! viewLastOutputDetails baseStep model dispatch ] ] ]
    | _, _ ->
        Quickview.body []
            [ Content.content [ Content.CustomClass "quickview-content--with-padding" ]
                [ R.str "The step could not be found. Please try to refresh you browser :(" ] ]

let viewWorkflowPane model dispatch =
    let hideQuickView _ = dispatch HideQuickView
    let closeModal _ = dispatch CloseGraphModal

    [ R.h1 [ RP.Class "is-size-3" ] [ R.str model.Workspace.Value.Name ]
      R.hr []
      Container.container [ Container.IsFluid ]
          (viewGraphPanel model dispatch)
      Quickview.quickview [ Quickview.IsActive model.QuickViewModel.IsActive ]
        [ Quickview.header []
            [ Quickview.title [] [ R.str "Edit Step" ]
              Delete.delete [ Delete.OnClick hideQuickView ] [] ]
          (viewQuickViewBody model dispatch)
          Quickview.footer []
            [ Button.button [ Button.OnClick hideQuickView ]
                [ R.str "Close" ] ] ]
      Modal.modal [ Modal.IsActive model.GraphModalModel.IsActive ]
        [ Modal.background [ Props [ RP.OnClick closeModal ] ] []
          Modal.Card.card []
            [ Modal.Card.head []
                [ Modal.Card.title [] [ R.str "Graph Display" ]
                  Delete.delete [ Delete.OnClick closeModal ] [] ]
              Modal.Card.body []
                [ Content.content []
                    [ R.div [ RP.Id "graph-modal-cy-container" ] [] ] ]
              Modal.Card.foot []
                [ Button.button [ Button.OnClick closeModal ] [ R.str "Close" ] ] ] ] ]

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
