#load @"referencesnetstd.fsx"
#load "Shared/Domain.fs"

open System
open Domain
open Domain.Model
open Domain.Commands
open System.Net
open System.CodeDom.Compiler

let userGuid = Guid.Parse "33541660-08a9-4faa-b44f-65d2e23294be"

let user =
    { User.Id = userGuid
      User.Email = "gregor@test.com"
      User.Name = "Gregor" }

let createUserSapceCmd = CreateUserSpace user

let ev = Behaviour.handleCommand [] createUserSapceCmd

let ws =
    { Workspace.Id = Guid.NewGuid ()
      Workspace.Name = "A test workspace"
      Workflow = None }
let createWorksSpaceCmd = CreateWorkSpace ws

let ev2 = Behaviour.handleCommand [ ev ] createWorksSpaceCmd

let events = [ ev; ev2 ]
let initUs = Domain.Projection.initialUserSpace events
let currentUs = Domain.Projection.userSpacesState events initUs.Value

let duplicateWs =
    { Workspace.Id = Guid.NewGuid ()
      Workspace.Name = "A test workspace"
      Workflow  = None }
let cwsCmd = CreateWorkSpace duplicateWs

currentUs.Workspaces
|> List.exists (fun ws -> ws.Name = duplicateWs.Name)

let ev3 = Behaviour.handleCommand events cwsCmd
let events2 = events@[ev3]

let ws2 =
    { duplicateWs with Name = "A unique name" }

let ev4 =
    CreateWorkSpace ws2
    |> Behaviour.handleCommand events2
let events3 = events@[ev4]

let currentUs2 = Domain.Projection.userSpacesState events3 initUs.Value

let ev5 =
    DeleteWorkSpace ws2
    |> Behaviour.handleCommand events3

let wf =
    Behaviour.newWorkflow ws.Id

let step1 =
    Behaviour.newWorkflowStep ()

let initializedFlow =
    { wf with WorkflowTree = Node (step1, []) }

let step2 = Behaviour.newWorkflowStep ()
let wft2 = Node (step2, [])

let wft3 = Behaviour.addStep step1 wft2 initializedFlow.WorkflowTree

let step3 = Behaviour.newWorkflowStep ()
let wft4 = Node (step3, [])

let wft5 = Behaviour.addStep step2 wft4 wft3

type Agent<'T> = MailboxProcessor<'T>

type WorkflowExecutorMsg =
    | Prepare of Workflow * AsyncReplyChannel<string list>
    | RegisterWorker of WorkflowTree
    | UnregisterWorker of Guid
    | RouteNextStep of WorkflowStep
    | RegisterOutput of string

type WorkerMsg =
    | ExecuteStep of WorkflowStep

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
            | ExecuteStep step when currentStep = step ->
                printfn "I, %O, have been called to execute my step" currentStep.Id

                // TODO here should be the logic for step execution
                match currentStep.StepType with
                | OutputStep ->
                    // might be the best to have an intermediary DU which is passed around
                    // the thinking is that it is routed to the next step
                    // the next step can take this as input
                    // the executor can inspect the routed values and log them
                    // it can also grab all values marked as output and cash them for the
                    // client
                    RegisterOutput "The output!!!!!" |> executorRef.Post
                | _ -> ()

                nextFlows
                |> List.iter
                    (fun flow ->
                        match flow with
                        | Empty -> failwith "Empty steps are not allowed"
                        | Node (step, _) ->
                            printfn "Next step to go is %O" step
                            RouteNextStep step |> executorRef.Post )

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

type ExecutorAgent() =

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

                    RouteNextStep firstStep |> agent.Post
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
            | RouteNextStep step ->
                workers
                |> List.iter (fun worker -> ExecuteStep step |> worker.CallWorker)
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

let reply = 
    use ex = new ExecutorAgent ()
    ex.Execute { wf with WorkflowTree = wft5 } |> Async.RunSynchronously
