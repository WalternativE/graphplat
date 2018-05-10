open System

type WorkflowStepState =
    | Prestine

type WorkflowStep =
    { Id : Guid
      State : WorkflowStepState }

type Workflow =
    | Empty
    | Node of WorkflowStep * Workflow list

let toNodesAndEdges (wf : Workflow) =
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

let iterWorkflow (f : WorkflowStep -> unit) (workflow : Workflow) =
    let rec iterWorkflow workflow =
        match workflow with
        | Empty -> ()
        | Node (step, wfs) ->
            f step
            wfs |> List.iter iterWorkflow

    iterWorkflow workflow

let addStep (parentStep : WorkflowStep) (flowToAdd : Workflow) (baseWorkflow : Workflow) =
    let rec addStep parentStep flowToAdd baseWorkflow =
        match baseWorkflow with
        | Empty -> Empty
        | Node (step, workflows) when step = parentStep ->
            Node (step, flowToAdd::workflows)
        | Node (step, workflows) ->
            Node (step, workflows |> List.map (addStep step flowToAdd))

    addStep parentStep flowToAdd baseWorkflow

let baseStepId = Guid.Parse "8c3e1670-5f32-4844-bf81-ef19c0f268ad"
let baseStep = { Id = baseStepId; State = Prestine }
let wf = Node (baseStep, [])

let secondStepId = Guid.Parse "e44bafda-f968-4a36-a394-d952b605656d"
let secondStep = { Id = secondStepId; State = Prestine }
let wf2 = Node (secondStep, [])

let compoundWf = addStep baseStep wf2 wf

let (nodes, edges) = toNodesAndEdges compoundWf

let doSomething (bla : 'a when 'a : equality) =
    bla

let aVal = 15
let anotherVal = "Hello World"
let afunc = (fun () -> ())

doSomething aVal
doSomething anotherVal
// doSomething afunc // this fails
