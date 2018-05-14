namespace Domain

open System

module Model =

    type UserId = Guid
    type Email = string
    type UserName = string

    type User =
        { Id : UserId
          Name : UserName
          Email : Email }

    type SimpleNode =
        { Id : int
          Label : string }

    type SimpleEdge =
        { From : int
          To : int
          Label : string }

    type SimpleGraph =
        { Nodes : SimpleNode list
          Edges : SimpleEdge list }

    type InputType =
        | EmptyInput
        | ConstantInput of SimpleGraph

    type StepType =
        | Unassigned
        | InputStep of InputType
        | ComputationStep
        | OutputStep

    type WorkflowStepState =
        | Prestine
        | Fixed

    type WorkflowStepId = Guid

    type WorkflowStep =
        { Id : Guid
          State : WorkflowStepState
          StepType : StepType }

    type WorkflowTree =
        | Empty
        | Node of WorkflowStep * WorkflowTree list

    type WorkflowId = Guid
    type WorkspaceId = Guid

    type Workflow =
        { Id: WorkflowId
          AssignedWorkspace : WorkspaceId
          WorkflowTree : WorkflowTree }

    type Workspace =
        { Id : WorkspaceId
          Name : string
          Workflow : Workflow option }

    type UserSpace =
        { Id : Guid
          User : User
          Workspaces : Workspace list }

module Events =

    open Model

    type UserError =
        | EmailAlreadyUsed of Email

    type UserEvent =
        | UserCreated of User
        | UserError of UserError

    type WorkflowError =
        | WorkflowAlreadyExists of Workflow
        | NoWorkspaceForWorkflow of Workflow
        | StepCouldNotBeAdded of WorkflowStep
        | StepCouldNotBeChanged of WorkflowStep

    let workflowErrorToString (e : WorkflowError) =
        match e with
        | WorkflowAlreadyExists wf -> sprintf "The workflow with the id %O already exists" wf.Id
        | NoWorkspaceForWorkflow wf -> sprintf "The workspace %O could not be found. It was not possible to add workflow %O to it." wf.AssignedWorkspace wf.Id
        | StepCouldNotBeAdded wfs -> sprintf "The workflowstep %O could not be added." wfs.Id
        | StepCouldNotBeChanged wfs -> sprintf "The workflowstep %O could not be changed." wfs.Id

    type Error =
        | AlreadyExists of UserSpace
        | NoUserSpace
        | WsNameNotUnique of Workspace
        | WsNotFound of Workspace

    let errorToString (e : Error) =
        match e with
        | AlreadyExists us -> sprintf "The userspace for user %s already exists." us.User.Name
        | NoUserSpace -> "No userspace exists which can be used for operations."
        | WsNameNotUnique ws -> sprintf """The name "%s" you chose for your new workspace is already used.""" ws.Name
        | WsNotFound ws -> sprintf "The workspace with the id %O could not be found." ws.Id

    type WorkflowEvent =
        | WorkflowCreated of Workflow
        | WorkflowError of WorkflowError
        | WorkflowStepAdded of Workflow
        | WorkflowStepChanged of Workflow

    type Event =
        | UserSpaceCreated of UserSpace
        | WorkspaceCreated of Workspace
        | WorkspaceDeleted of Workspace
        | WorkflowEvent of WorkflowEvent
        | Error of Error

module EventStore =

    type StreamedStoreEvent<'a> =
        { Id : Guid
          StreamId : Guid
          Event : 'a }

module Queries =

    open Model

    type UserAndWorkspaceId = UserId * WorkspaceId
    type UserAndWorkflowId = UserId * WorkflowId

    type UserQuery =
        | GetUser of UserId

    type QueryResult =
        | UserSpaceResult of UserSpace
        | WorkspaceResult of Workspace
        | WorkflowResult of Workflow

    type WorkflowQuery =
        | GetWorkflow of UserAndWorkflowId

    type SpacesQuery =
        | GetUserspace of UserId
        | GetWorkspace of UserAndWorkspaceId
        | WorkflowQuery of WorkflowQuery

module Commands =

    open Model

    type UserCommand =
        | CreateUser of User

    type AddStep =
        | AddFirst of WorkflowStep
        | AddStepAfter of WorkflowStep * WorkflowStep

    type WorkflowCommand =
        | CreateWorkflow of Workflow
        | AddStep of Workflow * AddStep
        // this is very broad - would be better to have more granular commands
        // still workable but more granular cmds seem to contain more infos (just an idea)
        | ChangeStep of WorkflowStep

    type SpacesCommand =
        | CreateUserSpace of User
        | CreateWorkSpace of Workspace
        | DeleteWorkSpace of Workspace
        | WorkflowCommand  of WorkflowCommand

module Projection =

    open Events
    open Model

    let state folder (givenHistory : 'b List) (model : 'c) =
        givenHistory
        |> List.fold folder model

    let applyToUser (user : User) (ue : UserEvent) =
        match ue with
        | UserCreated _ ->
            user
        | UserError _ ->
            user

    let userState = state applyToUser

    // TODO it appears stupid that I have to ways to handle this - both have their merits
    // I might want to merge the approaches or just dump one of them
    let applyToWorkflow (workflow : Workflow) (wev : WorkflowEvent) =
        match wev with
        | WorkflowCreated _ ->
            workflow
        | WorkflowStepAdded wf ->
            wf
        | WorkflowStepChanged wf ->
            wf
        | WorkflowError _ ->
            workflow

    let workflowState = state applyToWorkflow

    let applyWorkflowEvent (wfev : WorkflowEvent) (userSpace : UserSpace) =
        match wfev with
        | WorkflowCreated wf ->
            let wss =
                userSpace.Workspaces
                |> List.map (fun ws ->
                                if ws.Id = wf.AssignedWorkspace then
                                    { ws with Workflow = Some wf}
                                else ws )
            { userSpace with Workspaces = wss }
        | WorkflowStepAdded wf | WorkflowStepChanged wf ->
            let wss =
                userSpace.Workspaces
                |> List.map (fun ws ->
                                { ws with
                                    Workflow =
                                        ws.Workflow
                                        |> Option.map (fun workflow ->
                                                        if workflow.Id = wf.Id then wf else workflow ) } )
            { userSpace with Workspaces = wss }
        | WorkflowError _ ->
            userSpace

    let applyToUserSpace (userSpace : UserSpace) (e : Event) =
        match e with
        | UserSpaceCreated _ ->
            userSpace
        | WorkspaceCreated ws ->
            { userSpace with Workspaces = ws::userSpace.Workspaces }
        | WorkspaceDeleted ws ->
            let wss =
                userSpace.Workspaces
                |> List.filter (fun w -> w.Id = ws.Id |> not)
            { userSpace with Workspaces = wss }
        | WorkflowEvent wfev ->
            applyWorkflowEvent wfev userSpace
        | Error _ ->
            userSpace

    let userSpacesState = state applyToUserSpace

    let initialUserSpace (history : Event list) =
       match history with
       | [] -> None
       | x::_ ->
            match x with
            | UserSpaceCreated us -> Some us
            | _ -> None

module Behaviour =

    open Model
    open Commands
    open Projection
    open Events

    let createUserSpace (user : User) =
        { Id = user.Id
          User = user
          Workspaces = [] }

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
        let rec addStep (parentStep : WorkflowStep) flowToAdd baseWorkflow =
            match baseWorkflow with
            | Empty -> Empty
            | Node (step, workflows) when step.Id = parentStep.Id ->
                Node (step, flowToAdd::workflows)
            | Node (step, workflows) ->
                Node (step, workflows |> List.map (addStep parentStep flowToAdd))

        addStep parentStep flowToAdd baseWorkflow

    let newWorkflow (wsId : WorkspaceId) =
        { Id = Guid.NewGuid ()
          AssignedWorkspace = wsId
          WorkflowTree = Empty }

    let newWorkflowStep () =
        { Id = Guid.NewGuid ()
          State = Prestine
          StepType = Unassigned }

    let handleWorkflowCommand (us : UserSpace) (command : WorkflowCommand) =
        match command with
        | CreateWorkflow wf ->
            let wsToAttach =
                us.Workspaces
                |> List.tryFind (fun ws -> ws.Id = wf.AssignedWorkspace)

            match wsToAttach with
            | None -> NoWorkspaceForWorkflow wf |> WorkflowError
            | Some ws ->
                match ws.Workflow with
                | Some wf -> WorkflowAlreadyExists wf |> WorkflowError
                | None -> WorkflowCreated wf
        | AddStep (baseWf, mode) ->
            let doesBaseWorkflowExist =
                us.Workspaces
                |> List.filter (fun ws -> ws.Workflow.IsSome && ws.Workflow.Value = baseWf)
                |> (function
                    |[] -> false
                    |_ -> true)

            match mode with
            | AddStepAfter (parentStep, stepToAdd) ->
                if doesBaseWorkflowExist then
                    let wfToAdd = Node (stepToAdd, [])
                    let newWf = addStep parentStep wfToAdd baseWf.WorkflowTree
                    WorkflowStepAdded { baseWf with WorkflowTree = newWf }
                else
                    StepCouldNotBeAdded stepToAdd |> WorkflowError
            | AddFirst stepToAdd ->
                if doesBaseWorkflowExist then
                    let wfToAdd = Node (stepToAdd, [])
                    WorkflowStepAdded { baseWf with WorkflowTree = wfToAdd }
                else
                    StepCouldNotBeAdded stepToAdd |> WorkflowError
        | ChangeStep wfs ->
            let flows =
                us.Workspaces
                |> List.map (fun ws ->
                                ws.Workflow
                                |> Option.bind (fun wf ->
                                                    wf.WorkflowTree
                                                    |> tryFindWorkflowStep (fun step -> step.Id = wfs.Id)
                                                    |> Option.map (fun _ -> wf)))
                |> List.filter Option.isSome
                |> List.map Option.get
                |> List.tryHead

            match flows with
            | None -> StepCouldNotBeChanged wfs |> WorkflowError
            | Some wf ->
                wf.WorkflowTree
                |> mapWorkflowSteps (fun step -> if step.Id = wfs.Id then wfs else step)
                |> (fun wft -> { wf with WorkflowTree = wft})
                |> WorkflowStepChanged

    let handleCommand (events : Event list) (command : SpacesCommand) =
        match command with
        | CreateUserSpace user ->
            match initialUserSpace events with
            | Some us ->
                AlreadyExists us |> Error
            | None ->
                let s = createUserSpace user
                let ev = UserSpaceCreated s
                ev
        | CreateWorkSpace ws ->
            match initialUserSpace events with
            | None -> Error NoUserSpace
            | Some us ->
                let currentUs = userSpacesState events us
                let wsExists =
                    currentUs.Workspaces
                    |> List.exists (fun w -> w.Name = ws.Name)
                if wsExists then
                    WsNameNotUnique ws |> Error
                else
                    WorkspaceCreated ws
        | DeleteWorkSpace ws ->
            match initialUserSpace events with
            | None -> Error NoUserSpace
            | Some us ->
                let currentUs = userSpacesState events us
                let wsCanBeFound =
                    currentUs.Workspaces
                    |> List.exists (fun w -> w.Id = ws.Id)
                if wsCanBeFound then
                    WorkspaceDeleted ws
                else
                    WsNotFound ws |> Error
        | WorkflowCommand wfcmd ->
            let uso =
                initialUserSpace events
                |> Option.map (userSpacesState events)

            match uso with
            | None -> NoUserSpace |> Error
            | Some us ->
                handleWorkflowCommand us wfcmd
                |> WorkflowEvent
