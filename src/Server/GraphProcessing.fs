module GraphProcessing

open Domain.Model
open FSharp.FGL
open FSharp.FGL.Directed

let createTestGraph () =
    Graph.empty
    |> Vertices.addMany [
        (1, "One")
        (2, "Two")
        (3, "Three")
        (4, "Four")
        (5, "Five")
        (6, "Six")
        (7, "Seven")]
    |> Edges.addMany [
        (1, 4, "One Four")
        (1, 6, "One Six")
        (1, 5, "One Five")
        (2, 5, "Two Five")
        (2, 7, "Two Seven")
        (2, 6, "Two Six")
        (3, 7, "Three Seven")
        (3, 6, "Three Six")
        (4, 1, "Four One")
        (4, 7, "Four Seven")]

let createTestSimpleGraph () =
    ()