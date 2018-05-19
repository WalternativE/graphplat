namespace Computations

open System
open FSharp.FGL

type Graph = Graph<int, string, string>

type IComputationRepository =

    abstract member GetComputation : Guid -> (Graph -> Graph)

module private Internal =

    let dfs (nodes : int list) (graph : Graph) : int list =
        let rec dfs nodes graph =
            match nodes, graph with
            | ([], _) -> []
            | (_, g) when Graph.isEmpty g -> []
            | (n::ns, g) ->
                match Graph.tryDecompose n g with
                | (Some c, g') ->
                    let neighbours = Undirected.Vertices.neighbours c
                    n::(dfs (neighbours @ ns) g')
                | None, _ -> dfs ns g

        dfs nodes graph


    // I create this as fixed as currently all computations are hard coded
    // the concept of a repository currently only exists as a very constant thing
    let graphIdentId = Guid.Parse "c1a71409-f90a-4822-9720-93d3853ed3ab"
    let graphIdentity (g : Graph) = g


    let toEdge (v1 : LVertex<int, string>) (v2 : LVertex<int, string>) =
        let (v1id, _) = v1
        let (v2id, _) = v2
        (v1id, v2id, sprintf "from %i to %i" v1id v2id)


    let dfsSubgraph countToPick graph : Graph =
        let dfsOrder = dfs [1] graph

        let vertizes = 
            dfsOrder
            |> List.take countToPick
            |> List.map (fun node -> Directed.Vertices.find node graph )

        let edges =
            vertizes
            |> List.windowed 2
            |> List.map (fun vpair -> toEdge vpair.[0] vpair.[1])

        let graph = Graph.empty
        graph
        |> Directed.Vertices.addMany vertizes
        |> Directed.Edges.addMany edges


    // same reason - all those things are constant right now
    let dfsSubgraphId = Guid.Parse "9a98065c-83fd-4823-a9e7-dc11e250253a"
    let dfsSubgraphThreeNodes = dfsSubgraph 3


    type internal ConstantComputationRepository() =

        let lookupTable =
            [ (graphIdentId, graphIdentity)
              (dfsSubgraphId, dfsSubgraphThreeNodes) ]
            |> Map.ofList

        interface IComputationRepository with
            member this.GetComputation (id : Guid) =
                Map.find id lookupTable


module Repository =

    let getRepository () : IComputationRepository =
        Internal.ConstantComputationRepository() :> IComputationRepository
