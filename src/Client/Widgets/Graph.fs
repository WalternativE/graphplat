module Graph

open Fable.Core
open Fable.Core.JsInterop
open Elmish
open Cytoscape
open System
open Fable.Import.React
open Domain
open Domain.Model
open Domain.Commands
open Fulma

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props
module C = Cytoscape
module B = Fable.Import.Browser

type Model =
    { Id : string
      Core : C.Cytoscape.Core option
      Workflow : WorkflowTree }

type ExtMsg =
    | NoOp
    | NodeSelected of WorkflowStepId
    | AddStep of AddStep

type Msg =
    | ContainerAvailable of Fable.Import.Browser.HTMLElement * DispatchFunc
    | ContainerUnavailable
    | NodeTapped of WorkflowStepId
    | CreateFirstNodeClicked
and
    DispatchFunc = Msg -> unit

let init (wft : WorkflowTree) =
    let model =
        let guid = Guid.NewGuid()
        { Id = guid.ToString()
          Core = None
          Workflow = wft }
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

let createNode (id :string) =
    let eDef = createEmpty<C.Cytoscape.NodeDefinition>
    eDef.group <- Some C.Cytoscape.ElementGroup.Nodes
    let dataDef = createEmpty<C.Cytoscape.NodeDataDefinition>
    dataDef.id <- Some id
    eDef.data <- dataDef
    eDef

let createEdge (e : string * string) =
    let (id1, id2) = e
    let def = createEmpty<C.Cytoscape.EdgeDefinition>
    def.group <- Some C.Cytoscape.ElementGroup.Edges
    let data = createEmpty<C.Cytoscape.EdgeDataDefinition>
    data.source <- id1
    data.target <- id2
    def.data <- data
    def

let createGraph (ids : Set<string>) (edges : (string * string) list) =
    let nodes =
        ids
        |> Set.toList
        |> List.map createNode
        |> ResizeArray

    let edges =
        edges
        |> List.map createEdge
        |> ResizeArray

    let elsDef = createEmpty<C.Cytoscape.ElementsDefinition>
    elsDef.nodes <- nodes
    elsDef.edges <- edges
    elsDef

let handleNodeTap (dispatch : DispatchFunc) (eo : Cytoscape.EventObject) =
    match eo.target with
    | None -> ()
    | Some t -> !!t?id() |> NodeTapped |> dispatch

let graphWorkflowTree (wft : WorkflowTree) =
    wft
    |> toNodesAndEdges
    |> (fun (nodes, edges) -> createGraph nodes edges)

let update (msg : Msg) (model : Model) =
    match msg with
    | ContainerAvailable (el, dispatch) ->
        let graph =
            model.Workflow
            |> graphWorkflowTree

        let opts = createEmpty<C.Cytoscape.CytoscapeOptions>
        opts.container <- (Some el)
        opts.elements <- Some !^graph

        let core = C.cytoscape opts

        let handler : C.Cytoscape.EventHandler = handleNodeTap dispatch
        core.on ("tap", "node", handler)

        { model with Core = None }, Cmd.none, NoOp
    | ContainerUnavailable ->
        { model with Core = None }, Cmd.none, NoOp
    | NodeTapped wfsId ->
        { model with Core = None }, Cmd.none, NodeSelected wfsId
    | CreateFirstNodeClicked ->
        let step = Behaviour.newWorkflowStep ()
        let initialTree = Node (step, [])

        let addFirst = AddFirst step
        { model with Workflow = initialTree }, Cmd.none, AddStep addFirst

module IR = Fable.Import.React

type [<Pojo>] GraphProps =
    { Id : string
      Dispatch : Msg -> unit }

type GraphComponent(initialProps) =
    inherit IR.Component<GraphProps, obj>(initialProps)

    override this.componentDidMount() =
        let el = B.document.getElementById this.props.Id
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
