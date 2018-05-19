module Graph

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.React

open Elmish
open Fulma

open Domain
open Domain.Commands
open Domain.Model

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props
module B = Fable.Import.Browser
module IR = Fable.Import.React

module C = Cytoscape

type Model =
    { Id : string
      Core : C.Cytoscape.Core option
      Workflow : WorkflowTree
      RerenderHook : (unit -> unit) option }

type ExtMsg =
    | NoOp
    | NodeSelected of WorkflowStepId
    | AddStep of AddStep

type Msg =
    | ContainerAvailable of Fable.Import.Browser.HTMLElement * DispatchFunc
    | ContainerUnavailable
    | NodeTapped of WorkflowStepId
    | CreateFirstNodeClicked
    | ForceSync of WorkflowTree
    | RegisterRerenderHook of (unit -> unit)
    | ForceRerender
    | ReinitCore of Fable.Import.Browser.HTMLElement * DispatchFunc
and
    DispatchFunc = Msg -> unit

let init (wft : WorkflowTree) =
    let model =
        let guid = Guid.NewGuid()
        { Id = guid.ToString()
          Core = None
          Workflow = wft
          RerenderHook = None }
    model, Cmd.none

module B = Fable.Import.Browser

let toNodesAndEdges (wf : WorkflowTree) =
    let rec toNodesAndEdges current source wf =
        match source, wf with
        | None, Empty -> [], []
        | Some _, Empty -> [], []
        | Some s, Node (wsStep, wfs) ->
            let (nodes, edges) = current
            let nextSource = string wsStep.Id |> Some
            let edge = (s, string wsStep.Id)
            let cns = (string wsStep.Id)::nodes
            let edgs = edge::edges

            wfs
            |> List.map (fun workflow -> toNodesAndEdges (cns, edgs) nextSource workflow)
            |> List.fold (fun (cnn, cee) (nn, ee) -> cnn@nn, cee@ee ) (cns, edgs)

        | None, Node (wsStep, wfs) ->
            let (nodes, edges) = current
            let nextSource = string wsStep.Id |> Some
            let cns = (string wsStep.Id)::nodes

            wfs
            |> List.map (fun workflow -> toNodesAndEdges (cns, edges) nextSource workflow)
            |> List.fold (fun (cnn, cee) (nn, ee) -> cnn@nn, cee@ee ) (cns, edges)

    let (nodes, edges) =
        toNodesAndEdges ([], []) None wf

    Set.ofList nodes, edges

let handleNodeTap (dispatch : DispatchFunc) (eo : C.Cytoscape.EventObject) =
    match eo.target with
    | None -> ()
    | Some t -> !!t?id() |> NodeTapped |> dispatch

let graphWorkflowTree (wft : WorkflowTree) =
    wft
    |> toNodesAndEdges
    |> (fun (nodes, edges) -> C.Utilities.createGraph nodes edges)

let initCytoCore (wf : WorkflowTree) el dispatch =
    let graph =
        wf
        |> graphWorkflowTree

    let opts = createEmpty<C.Cytoscape.CytoscapeOptions>
    opts.container <- (Some el)
    opts.elements <- Some !^graph

    let core = C.cytoscape opts

    let handler : C.Cytoscape.EventHandler = handleNodeTap dispatch
    core.on ("tap", "node", handler)
    core

let update (msg : Msg) (model : Model) =
    match msg with
    | ContainerAvailable (el, dispatch) ->
        let core = initCytoCore model.Workflow el dispatch
        { model with Core = Some core }, Cmd.none, NoOp
    | ContainerUnavailable ->
        { model with Core = None }, Cmd.none, NoOp
    | NodeTapped wfsId ->
        { model with Core = None }, Cmd.none, NodeSelected wfsId
    | CreateFirstNodeClicked ->
        let step = Behaviour.newWorkflowStep ()
        let initialTree = Node (step, [])

        let addFirst = AddFirst step
        { model with Workflow = initialTree }, Cmd.none, AddStep addFirst
    | ForceSync workflowTree  ->
        { model with Workflow = workflowTree }, Cmd.ofMsg ForceRerender, NoOp
    | RegisterRerenderHook f ->
        { model with RerenderHook = Some f}, Cmd.none, NoOp
    | ForceRerender ->
        match model.RerenderHook with
        | None -> ()
        | Some f ->
            B.console.log "Force rerender"
            f ()
        model, Cmd.none, NoOp
    | ReinitCore (el, dispatch) ->
        let core = initCytoCore model.Workflow el dispatch
        { model with Core = Some core}, Cmd.none, NoOp

type [<Pojo>] GraphProps =
    { Id : string
      Dispatch : Msg -> unit }

type GraphComponent(initialProps) =
    inherit IR.Component<GraphProps, obj>(initialProps)

    member this.reinitCore() =
        let el = B.document.getElementById this.props.Id
        ReinitCore (el, this.props.Dispatch) |> this.props.Dispatch

    override this.componentDidMount() =
        let el = B.document.getElementById this.props.Id
        RegisterRerenderHook this.reinitCore |> this.props.Dispatch
        ContainerAvailable (el, this.props.Dispatch) |> this.props.Dispatch

    override this.render () =
        R.div [ RP.Id this.props.Id
                RP.Style [ RP.MinHeight "400px" ] ] []

let inline graph props children = R.ofType<GraphComponent, _, _> props children

let view (model : Model) (dispatch : Msg -> unit) =
    match model.Workflow with
    | Empty ->
        Content.content []
            [ Button.button
                [ Button.Color IsPrimary
                  Button.IsFullWidth
                  Button.OnClick (fun _ -> dispatch CreateFirstNodeClicked ) ]
                [ R.str "Click here to create your first node!" ] ]
    | Node _ -> graph { Id = model.Id; Dispatch = dispatch } []
