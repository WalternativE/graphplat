#load @"referencesnetstd.fsx"
#load "Shared/Domain.fs"

open System
open Domain
open Domain.Model
open Domain.Commands
open System.Net

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
    | Prepare of Workflow * AsyncReplyChannel<string option>
    | RegisterWorker of WorkflowTree
    | UnregisterWorker of Guid

type WorkerMsg =
    | WorkStep of AsyncReplyChannel<unit>

type ExecutorAgent() =

    let eventSrc = new Event<_>()

    let agent = Agent.Start(fun agent ->

        let rec prepare workers =
            agent.Scan (fun msg ->
                match msg with
                | Prepare (wf, reply) -> Some <| async {
                    wf.WorkflowTree
                    |> Behaviour.iterWorkflowTree (fun wft -> agent.Post <| RegisterWorker wft)
                    return! execute workers }
                | _ -> None
            )
        and execute workers = async {
            let! msg = agent.Receive()
            match msg with
            | RegisterWorker wft ->
                printfn "I have %i workers right now" <| List.length workers
                let worker = WorkerAgent (wft, agent)
                return! execute (worker::workers)
            | _ -> return! execute workers
        }

        prepare []
    )

    member this.Execute (wf : Workflow) =
        agent.PostAndAsyncReply (fun ch -> Prepare (wf, ch))
and
    WorkerAgent (wft : WorkflowTree, executorRef) =

        let agent = Agent.Start(fun agent ->
            let rec work () = async {
                let! msg = agent.Receive ()
                match msg with
                | WorkStep (reply) -> return! work()
            }

            work ()
        )

let ex = ExecutorAgent ()

let str = 
    ex.Execute { wf with WorkflowTree = wft5 } |> Async.RunSynchronously
