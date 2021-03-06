#r @"C:\Users\Gregor\.nuget\packages\aether\8.2.0\lib\netstandard1.6\Aether.dll"
#r @"Server/libs/FSharp.FGL/FSharp.FGL.dll"

open FSharp.FGL

let g =
    Graph.empty
    |> Directed.Vertices.addMany [
        (1, "One")
        (2, "Two")
        (3, "Three")
        (4, "Four")
        (5, "Five")
        (6, "Six")
        (7, "Seven")]
    |> Directed.Edges.addMany [
        (1, 4, "One Four")
        (1, 6, "One Six")
        (1, 5, "One Five")
        (2, 5, "Two Five")
        (2, 7, "Two Seven")
        (2, 6, "Two Six")
        (3, 7, "Three Seven")
        (3, 6, "Three Six")
        (4, 7, "Four Seven")]

Directed.Vertices.tovertexList g
Directed.Edges.iter (fun v ov e -> printfn "vertex1 %A vertex2 %A edge %A" v ov e) g

Directed.Graph.toAdjacencyMatrix g

Graph.decompose 1 g

#load "Shared/Domain.fs"
open Domain.Model

let toSimpleGraph (fglGraph : Graph<int, string, string>) : SimpleGraph =
    let vertizes =
        fglGraph
        |> Directed.Vertices.tovertexList
        |> List.map (fun (node, label) -> { Id = node; Label = label } )

    let edges =
        vertizes
        |> List.map
            (fun node ->
                let ((_, node, _, oa), _) = Graph.decompose node.Id fglGraph
                oa
                |> List.map
                    (fun (otherNode, edgeLabel) ->
                        { From = node
                          To = otherNode
                          Label = edgeLabel } ) )
        |> List.collect id

    { Nodes = vertizes; Edges = edges }

toSimpleGraph g
