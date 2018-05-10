namespace Domain

open System

module Model =

    type Email = string
    type UserName = string

    type User =
        { Id : Guid
          Name : UserName
          Email : Email }

    type WorkflowStepState =
        | Prestine

    type WorkflowStep =
        { Id : Guid
          State : WorkflowStepState }

    type WorkflowTree =
        | Empty
        | Node of WorkflowStep * WorkflowTree list

    type Workflow =
        { Id: Guid
          AssignedWorkspace : Guid
          WorkflowTree : WorkflowTree }

    type Workspace =
        { Id : Guid
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

    type Event =
        | UserSpaceCreated of UserSpace
        | WorkspaceCreated of Workspace
        | WorkspaceDeleted of Workspace
        | WorkflowEvent of WorkflowEvent
        | Error of Error
open Events

module EventStore =

    type StreamedStoreEvent<'a> =
        { Id : Guid
          StreamId : Guid
          Event : 'a }

module Queries =

    open Model

    type UserQuery =
        | GetUser of Guid

    type UserAndWorkspaceId = Guid * Guid

    type QueryResult =
        | UserSpaceResult of UserSpace
        | WorkspaceResult of Workspace

    type SpacesQuery =
        | GetUserspace of Guid
        | GetWorkspace of UserAndWorkspaceId

module Commands =

    open Model

    type UserCommand =
        | CreateUser of User

    type WorkflowCommand =
        | CreateWorkflow of Workflow

    type SpacesCommand =
        | CreateUserSpace of User
        | CreateWorkSpace of Workspace
        | DeleteWorkSpace of Workspace
        | WorkflowCommand  of WorkflowCommand

module Projection =

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

    let applyToWorkflow (workflow : Workflow) (wev : WorkflowEvent) =
        match wev with
        | WorkflowCreated _ ->
            workflow
        | WorkflowError _ ->
            workflow

    let workflowState = state applyToWorkflow

    let initialWorkflow (evs : WorkflowEvent list) =
        match evs with
        | [] -> None
        | initialEvent::_ ->
            match initialEvent with
            | WorkflowCreated wf -> Some wf
            | _ -> None

    let applyWorkflowEvent (wfev : WorkflowEvent) (userspace : UserSpace) =
        match wfev with
        | WorkflowCreated wf ->
            let wss =
                userspace.Workspaces
                |> List.map (fun ws ->
                                if ws.Id = wf.AssignedWorkspace then
                                    { ws with Workflow = Some wf}
                                else ws )
            { userspace with Workspaces = wss }
        | WorkflowError _ ->
            userspace

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

    let createUserSpace (user : User) =
        { Id = user.Id
          User = user
          Workspaces = [] }

    let iterWorkflow (f : WorkflowStep -> unit) (workflow : WorkflowTree) =
        let rec iterWorkflow workflow =
            match workflow with
            | Empty -> ()
            | Node (step, wfs) ->
                f step
                wfs |> List.iter iterWorkflow

        iterWorkflow workflow

    let addStep (parentStep : WorkflowStep) (flowToAdd : WorkflowTree) (baseWorkflow : WorkflowTree) =
        let rec addStep parentStep flowToAdd baseWorkflow =
            match baseWorkflow with
            | Empty -> Empty
            | Node (step, workflows) when step = parentStep ->
                Node (step, flowToAdd::workflows)
            | Node (step, workflows) ->
                Node (step, workflows |> List.map (addStep step flowToAdd))

        addStep parentStep flowToAdd baseWorkflow

    let handleWorkflowCommand (events : Event list) (command : WorkflowCommand) =
        let wfHistory =
            events
            |> List.map (fun ev ->
                                match ev with
                                | WorkflowEvent wfe -> Some wfe
                                | _ -> None )
            |> List.filter Option.isSome
            |> List.map (fun o -> o.Value)

        match command with
        | CreateWorkflow wf ->
            match initialWorkflow  wfHistory with
            | None -> WorkflowCreated wf
            | Some _ -> WorkflowAlreadyExists wf |> WorkflowError

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
            handleWorkflowCommand events wfcmd
            |> WorkflowEvent
