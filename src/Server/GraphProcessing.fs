module GraphProcessing

open System
open FSharp.FGL
open Domain
open Domain.Model
open Computations

type StepValue =
    | Unit
    | Scalar of double
    | Graph of Graph<int, string, string>
    | ExecutionError of string

type LabeledOutput =
    { Label : string
      Output : StepValue }

// I want to have a registry for computations
// initially this will be hardcoded in the future it will be a service
// where I register functions/containers - the computation id will be bound to computation block
let getComputationMethod (compId : Guid) =
    let repo =Repository.getRepository ()
    let computation = repo.GetComputation compId

    (fun (value : StepValue) ->
        match value with
        | Graph g ->
            computation g
            |> Graph
        | _ -> ExecutionError "This method is not applicable to the type you passed.")

// same as with inputs - I'd like to also reference the output collection of other
// workflows as possible inuts (at least that currently appears to be sensible)
let getInputMethod (inputId : Guid) =
    // right now it's also always a constant value
    let constantGraph =
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
    constantGraph
    |> Graph

type Agent<'T> = MailboxProcessor<'T>

type WorkflowExecutorMsg =
    | Prepare of Workflow * AsyncReplyChannel<LabeledOutput list>
    | RegisterWorker of WorkflowTree
    | UnregisterWorker of Guid
    | RouteNextStep of StepValue * WorkflowStep
    | RegisterOutput of LabeledOutput

type WorkerMsg =
    | ExecuteStep of StepValue * WorkflowStep

type WorkerAgent (wft : WorkflowTree, executorRef : Agent<WorkflowExecutorMsg>) =
    let workerId =
        Guid.NewGuid ()

    let (currentStep, nextFlows) =
        match wft with
        | Empty -> failwith "Workflows with empty nodes cannot be executed!"
        | Node (step, flows) ->
            (step, flows)

    let agent = Agent.Start(fun agent ->
        let rec work () = async {
            let! msg = agent.Receive ()
            match msg with
            | ExecuteStep (value, step) when currentStep = step ->
                printfn "I, %O, have been called to execute my step" currentStep.Id

                let nextParam =
                    match currentStep.StepType with
                    | OutputStep ->
                        // might be the best to have an intermediary DU which is passed around
                        // the thinking is that it is routed to the next step
                        // the next step can take this as input
                        // the executor can inspect the routed values and log them
                        // it can also grab all values marked as output and cash them for the
                        // client
                        RegisterOutput { Label = "GenericOutputLabel"; Output = value } |> executorRef.Post
                        Unit
                    | InputStep inputType ->
                        // TODO this should contain some sort of logic
                        Guid.NewGuid () |> getInputMethod 
                    | ComputationStep ->
                        // TODO this should also contain logic
                        let computation =
                           Guid.Parse "9a98065c-83fd-4823-a9e7-dc11e250253a"
                           |> getComputationMethod
                        computation value
                    | Unassigned ->
                        failwith "An unassigned step should never be run"

                nextFlows
                |> List.iter
                    (fun flow ->
                        match flow with
                        | Empty -> failwith "Empty steps are not allowed"
                        | Node (step, _) ->
                            printfn "Next step to go is %O" step
                            RouteNextStep (nextParam, step) |> executorRef.Post )

                UnregisterWorker workerId |> executorRef.Post

                return! work ()
            | ExecuteStep _ ->
                return! work ()
        }

        work ()
    )

    member this.WorkerId = workerId

    member this.CallWorker (msg : WorkerMsg) =
        agent.Post msg

    interface IDisposable with
        member this.Dispose () =
            let disposableAgent = agent :> IDisposable
            disposableAgent.Dispose()

type ExecutorAgent () =

    let agent = Agent.Start(fun agent ->
        let rec prepare workers =
            agent.Scan (fun msg ->
                match msg with
                | Prepare (wf, reply) -> Some <| async {
                    let firstStep =
                        match wf.WorkflowTree with
                        | Empty -> failwith "Executing a workflow with empty nodes is not possible"
                        | Node (step, _) ->
                            step

                    wf.WorkflowTree
                    |> Behaviour.iterWorkflowTree (fun wft ->
                                                    RegisterWorker wft
                                                    |> agent.Post)

                    RouteNextStep (Unit, firstStep) |> agent.Post
                    return! execute reply workers [] }
                | _ -> None
            )
        and execute reply workers outputs = async {
            let execReply = execute reply

            let! msg = agent.Receive()
            match msg with
            | RegisterWorker wft ->
                printfn "I have %i workers right now" <| List.length workers
                let worker = new WorkerAgent (wft, agent)
                return! execReply (worker::workers) outputs
            | UnregisterWorker workerId ->
                let wrkrs =
                    workers
                    |> List.filter (fun w ->
                        if w.WorkerId <> workerId then
                            true
                        else
                            let disposableWorker = w :> IDisposable
                            disposableWorker.Dispose ()
                            false)
                printfn "I have %i workers left" <| List.length wrkrs

                match wrkrs with
                | [] ->
                    printfn "All done - sending reply"
                    reply.Reply outputs
                | wrkrs ->
                    return! execReply wrkrs outputs
            | RouteNextStep (parameter, step) ->
                workers
                |> List.iter (fun worker -> ExecuteStep (parameter, step) |> worker.CallWorker)
                return! execReply workers outputs
            | RegisterOutput op ->
                return! execReply workers (op::outputs)
            | Prepare _ -> return! execReply workers outputs
        }

        prepare []
    )

    member this.Execute (wf : Workflow) =
        agent.PostAndAsyncReply (fun channel -> Prepare (wf, channel))

    interface IDisposable with
        member this.Dispose() =
            printfn "Disposing Executor"
            let disposableAgent = agent :> IDisposable
            disposableAgent.Dispose ()

