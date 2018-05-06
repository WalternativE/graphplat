module Graph

open Fable.Core
open Fable.Core.JsInterop
open Elmish
open Cytoscape
open System
open Fable.Import.React

module R = Fable.Helpers.React
module C = Cytoscape

type Model =
    { Id : string
      Core : C.Cytoscape.Core option }

type Msg =
    | ContainerAvailable of Fable.Import.Browser.HTMLElement
    | ContainerUnavailable

let init () : Model * Cmd<Msg> =
    let model =
        let guid = Guid.NewGuid()
        { Id = guid.ToString(); Core = None }
    model, Cmd.none

module B = Fable.Import.Browser

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | ContainerAvailable el ->
        printfn "%A" C.cytoscape

        let opts = createEmpty<C.Cytoscape.CytoscapeOptions>
        opts.container <- (Some el)
        let core = C.cytoscape opts

        // just for dev
        printfn "%A" core

        { model with Core = None }, Cmd.none
    | ContainerUnavailable ->
        { model with Core = None }, Cmd.none

module IR = Fable.Import.React

type [<Pojo>] GraphProps =
    { Id : string
      Dispatch : Msg -> unit }

type GraphComponent(initialProps) =
    inherit IR.Component<GraphProps, obj>(initialProps)

    override this.componentDidMount() =
        let el = B.document.getElementById this.props.Id
        ContainerAvailable el |> this.props.Dispatch

    override this.render () =
        R.div [ R.Props.Id this.props.Id ] []

let inline graph props children = R.ofType<GraphComponent, _, _> props children

let view (model : Model) (dispatch : Msg -> unit) =
    graph { Id = model.Id; Dispatch = dispatch } []
