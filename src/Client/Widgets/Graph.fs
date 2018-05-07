module Graph

open Fable.Core
open Fable.Core.JsInterop
open Elmish
open Cytoscape
open System
open Fable.Import.React

module R = Fable.Helpers.React
module RP = Fable.Helpers.React.Props
module C = Cytoscape
module B = Fable.Import.Browser

type Model =
    { Id : string
      Core : C.Cytoscape.Core option }

type ExtMsg =
    | NoOp
    | NodeSelected

type Msg =
    | ContainerAvailable of Fable.Import.Browser.HTMLElement * DispatchFunc
    | ContainerUnavailable
    | NodeTapped
and
    DispatchFunc = Msg -> unit

let init () : Model * Cmd<Msg> =
    let model =
        let guid = Guid.NewGuid()
        { Id = guid.ToString(); Core = None }
    model, Cmd.none

module B = Fable.Import.Browser

let createNode (id :int) =
    let eDef = createEmpty<C.Cytoscape.NodeDefinition>
    eDef.group <- Some C.Cytoscape.ElementGroup.Nodes
    let dataDef = createEmpty<C.Cytoscape.NodeDataDefinition>
    dataDef.id <- string id |> Some
    eDef.data <- dataDef
    eDef

let createEdge (e : int * int) =
    let (id1, id2) = e
    let def = createEmpty<C.Cytoscape.EdgeDefinition>
    def.group <- Some C.Cytoscape.ElementGroup.Edges
    let data = createEmpty<C.Cytoscape.EdgeDataDefinition>
    data.source <- string id1
    data.target <- string id2
    def.data <- data
    def

let createGraph (edges : List<int * int>) (ids : Set<int>) =
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
    B.console.log eo
    dispatch NodeTapped

let update (msg : Msg) (model : Model) =
    match msg with
    | ContainerAvailable (el, dispatch) ->
        let graph =
            [1; 2; 3; 4; 5]
            |> Set.ofList
            |> createGraph [(1, 2); (2, 3); (3, 4); (4, 5)]

        let opts = createEmpty<C.Cytoscape.CytoscapeOptions>
        opts.container <- (Some el)
        opts.elements <- Some !^graph

        let core = C.cytoscape opts

        let handler : C.Cytoscape.EventHandler = handleNodeTap dispatch
        core.on ("tap", "node", handler)

        { model with Core = None }, Cmd.none, NoOp
    | ContainerUnavailable ->
        { model with Core = None }, Cmd.none, NoOp
    | NodeTapped ->
        { model with Core = None }, Cmd.none, NodeSelected

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
    graph { Id = model.Id; Dispatch = dispatch } []
