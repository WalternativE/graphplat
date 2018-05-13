open System

type WorkflowStepState =
    | Prestine

type WorkflowStepId = Guid

type WorkflowStep =
    { Id : Guid
      State : WorkflowStepState }

type WorkflowTree =
    | Empty
    | Node of WorkflowStep * WorkflowTree list

type WorkflowId = Guid
type WorkspaceId = Guid

type Workflow =
    { Id: WorkflowId
      AssignedWorkspace : WorkspaceId
      WorkflowTree : WorkflowTree }

let iterWorkflowSteps (f : WorkflowStep -> unit) (workflow : WorkflowTree) =
    let rec iterWorkflow workflow =
        match workflow with
        | Empty -> ()
        | Node (step, wfs) ->
            f step
            wfs |> List.iter iterWorkflow

    iterWorkflow workflow

let mapWorkflowSteps (f : WorkflowStep -> WorkflowStep) (workflow : WorkflowTree) =
    let rec mapWorkflowSteps workflow =
        match workflow with
        | Empty -> Empty
        | Node (step, wfs) ->
            Node (f step, wfs |> List.map mapWorkflowSteps)

    mapWorkflowSteps workflow

// TODO this gets the first occurence of a workflow step - there could be more than one 'equal steps' to get all paths collected
// I might get rid of this unwanted behaviour with a collector component which terminates a path and messages a waiting node
// have to ponder this
let tryFindWorkflowStep (predicate : WorkflowStep -> bool) (workflow : WorkflowTree) =
    let rec tryFindWorkflowStep workflow =
        match workflow with
        | Empty -> None
        | Node (step, []) ->
            if predicate step then
                Some step
            else
                None
        | Node (step, wfs) ->
            if predicate step then
                Some step
            else
                wfs
                |> List.map (fun wf -> tryFindWorkflowStep wf)
                |> List.filter Option.isSome
                |> List.map Option.get
                |> List.tryHead

    tryFindWorkflowStep workflow

let addStep (parentStep : WorkflowStep) (flowToAdd : WorkflowTree) (baseWorkflow : WorkflowTree) =
    let rec addStep parentStep flowToAdd baseWorkflow =
        match baseWorkflow with
        | Empty -> Empty
        | Node (step, workflows) when step = parentStep ->
            Node (step, flowToAdd::workflows)
        | Node (step, workflows) ->
            Node (step, workflows |> List.map (addStep step flowToAdd))

    addStep parentStep flowToAdd baseWorkflow

let newWorkflow (wsId : WorkspaceId) =
    { Id = Guid.NewGuid ()
      AssignedWorkspace = wsId
      WorkflowTree = Empty }

let newWorkflowStep () =
    { Id = Guid.NewGuid ()
      State = Prestine }
let baseStepId = Guid.Parse "8c3e1670-5f32-4844-bf81-ef19c0f268ad"
let baseStep = { Id = baseStepId; State = Prestine }
let wf = Node (baseStep, [])

let secondStepId = Guid.Parse "e44bafda-f968-4a36-a394-d952b605656d"
let secondStep = { Id = secondStepId; State = Prestine }
let wf2 = Node (secondStep, [])

let compoundWf = addStep baseStep wf2 wf

let bogusId = Guid.Parse "b089447a-5044-47df-b04f-447f499ff5f4"
let foundNode = tryFindWorkflowStep (fun step -> step.Id = bogusId) compoundWf

#r @"C:\Users\Gregor\.nuget\packages\aether\8.2.0\lib\netstandard1.6\Aether.dll"
#r @"Server/libs/FSharp.FGL/FSharp.FGL.dll"

open FSharp.FGL

let g =
    Graph.empty
    |> Undirected.Vertices.addMany [
        (1, "One")
        (2, "Two")
        (3, "Three")
        (4, "Four")
        (5, "Five")
        (6, "Six")
        (7, "Seven")]
    |> Undirected.Edges.addMany [
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

Undirected.Graph.toAdjacencyMatrix g
