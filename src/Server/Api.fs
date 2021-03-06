module Api

open System
open System.IO
open System.Security.Claims
open Microsoft.AspNetCore.Http

open Giraffe
open Saturn

open Auth
open Shared
open Domain
open Domain.Queries
open Domain.Commands
open Domain.Model
open Domain.Events
open DataAccess
open GraphProcessing

[<Literal>]
let connectionString = "Host=localhost;Port=5432;Username=postgres;Password=admin123;Database=graphplat"
let spacesEventStore : EventStore<Event> = MartenStore.eventStore connectionString
let queryHandler = CQHandlers.spacesQueryHandler spacesEventStore

let getUserId (ctx : HttpContext) =
    let idClaim = ctx.User.FindFirst ClaimTypes.NameIdentifier
    Guid.Parse idClaim.Value

let getUser (ctx : HttpContext) =
    let name = ctx.User.FindFirst ClaimTypes.GivenName
    let uid = getUserId ctx
    let mail = ctx.User.FindFirst ClaimTypes.Email
    { Id = uid ; Name = name.Value; Email = mail.Value }

let getCurrentUser next (ctx : HttpContext) = task {
    let user = getUser ctx
    return! json user next ctx
}

let commandHandler (streamId : Guid) (cmd : SpacesCommand) =
    let history = spacesEventStore.getEvents streamId
    let ev = Behaviour.handleCommand history cmd
    spacesEventStore.writeEvent streamId ev
    ev

let handleUnexpectedBehavior =
    setStatusCode 520 >=> text "An unexpected application state was created."

let handlePostToken (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<LoginCredentials>()

    let user = authenticateUser model
    match user with
    | Some u ->
        let tokenResult = { Token = generateToken u }
        return! json tokenResult nxt ctx
    | None ->
        let resp = setStatusCode 401 >=> text "Bad credentials"
        return! resp nxt ctx }

let getUserSpace (nxt : HttpFunc) (ctx : HttpContext) = task {
    let uid = getUserId ctx
    let space =
        GetUserspace uid
        |> queryHandler

    match space with
    | None ->
        let resp = setStatusCode 404 >=> text "Not found"
        return! resp nxt ctx
    | Some (UserSpaceResult usp) ->
        return! json usp nxt ctx
    | Some _ ->
        return! handleUnexpectedBehavior nxt ctx
}

let createUserSpace (nxt : HttpFunc) (ctx : HttpContext) = task {
    let user = getUser ctx
    let ev =
        CreateUserSpace user
        |> commandHandler user.Id

    match ev with
    | UserSpaceCreated usp ->
        let resp = setStatusCode 201 >=> json usp
        return! resp nxt ctx
    | Error e ->
        let resp = setStatusCode 400 >=> text (errorToString e)
        return! resp nxt ctx
    | _ ->
        return! handleUnexpectedBehavior nxt ctx
}

let createWorkspace (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<Workspace>()
    let user = getUser ctx

    let ev =
        CreateWorkSpace model
        |> commandHandler user.Id

    match ev with
    | WorkspaceCreated ws ->
        let resp = setStatusCode 201 >=> json ws
        return! resp nxt ctx
    | Error e ->
        let resp = setStatusCode 400 >=> text (errorToString e)
        return! resp nxt ctx
    | _ ->
        return! handleUnexpectedBehavior nxt ctx
}

let deleteWorkSpace (workspaceId : Guid) (nxt : HttpFunc) (ctx : HttpContext) = task {
    let ws =
        { Id = workspaceId;
          Name = ""
          Workflow = None }
    let user = getUser ctx

    let ev =
        DeleteWorkSpace ws
        |> commandHandler user.Id

    match ev with
    | WorkspaceDeleted _ ->
        return! setStatusCode 200 nxt ctx
    | Error e ->
        let error = errorToString e
        match e with
        | WsNotFound _ ->
            let resp = setStatusCode 404 >=> text error
            return! resp nxt ctx
        | _ ->
            let resp = setStatusCode 400 >=> text error
            return! resp nxt ctx
    | _ -> return! handleUnexpectedBehavior nxt ctx
}

let getWorkspace (workspaceId : Guid) (nxt : HttpFunc) (ctx : HttpContext) = task {
    let user = getUser ctx

    let queryResult =
        GetWorkspace (user.Id, workspaceId)
        |> queryHandler

    match queryResult with
    | None ->
        let resp = setStatusCode 404 >=> text "The requested workspace could not be found"
        return! resp nxt ctx
    | Some (WorkspaceResult ws) ->
        return! json ws nxt ctx
    | Some _ ->
        return! handleUnexpectedBehavior nxt ctx
}

let getWorkflow (workflowId : Guid) (nxt : HttpFunc) (ctx : HttpContext) = task {
    let user = getUser ctx

    let queryResult =
        GetWorkflow (user.Id, workflowId)
        |> WorkflowQuery
        |> queryHandler

    match queryResult with
    | None ->
        let resp = setStatusCode 404 >=> text "The requested workspace could not be found"
        return! resp nxt ctx
    | Some (WorkflowResult wf) ->
        return! json wf nxt ctx
    | Some _ ->
        return! handleUnexpectedBehavior nxt ctx
}

let createWorkflow (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<Workflow>()
    let user = getUser ctx

    let createEvent =
        CreateWorkflow model
        |> WorkflowCommand
        |> commandHandler user.Id

    match createEvent with
    | WorkflowEvent (WorkflowError e) ->
        let errorText = workflowErrorToString e
        match e with
        | WorkflowAlreadyExists _ ->
            let resp = setStatusCode 409 >=>  text errorText
            return! resp nxt ctx
        | NoWorkspaceForWorkflow _ ->
            let resp = setStatusCode 400 >=> text errorText
            return! resp nxt ctx
        | _ -> return! handleUnexpectedBehavior nxt ctx
    | WorkflowEvent (WorkflowCreated wf) ->
        let resp = setStatusCode 201 >=> json wf
        return! resp nxt ctx
    | _ -> return! handleUnexpectedBehavior nxt ctx
}

let handleAddStep (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<WorkflowCommand>()
    let user = getUser ctx

    match model with
    | AddStep _ ->
        let evt =
            model
            |> WorkflowCommand
            |> commandHandler user.Id

        match evt with
        | WorkflowEvent (WorkflowStepAdded wf) ->
            let resp = setStatusCode 201 >=> json wf
            return! resp nxt ctx
        | WorkflowEvent (WorkflowError e) ->
            let resp = setStatusCode 400 >=> text (workflowErrorToString e)
            return! resp nxt ctx
        | _ -> return! handleUnexpectedBehavior nxt ctx
    | _ ->
        let resp = setStatusCode 400 >=> text "Expected to get an AddStep command but received something else."
        return! resp nxt ctx
}

let handleChangeStep (nxt : HttpFunc) (ctx : HttpContext) = task {
    let! model = ctx.BindJsonAsync<WorkflowCommand>()
    let user = getUser ctx

    match model with
    | ChangeStep _ ->
        let evt =
            model
            |> WorkflowCommand
            |> commandHandler user.Id

        match evt with
        | WorkflowEvent (WorkflowStepChanged wf) ->
            let resp = setStatusCode 200 >=> json wf
            return! resp nxt ctx
        | WorkflowEvent (WorkflowError e) ->
            let resp = setStatusCode 400 >=>  text (workflowErrorToString e)
            return! resp nxt ctx
        | _ -> return! handleUnexpectedBehavior nxt ctx
    | _ ->
        let resp = setStatusCode 400 >=> text "Expected ChangeStep command but received something else."
        return! resp nxt ctx
}

let executeWorkflow (workflowId : Guid) (nxt : HttpFunc) (ctx : HttpContext) = task {
    let user = getUser ctx

    let queryResult =
        GetWorkflow (user.Id, workflowId)
        |> WorkflowQuery
        |> queryHandler

    match queryResult with
    | None ->
        let resp = setStatusCode 404 >=> text "The requested workflow could not be found"
        return! resp nxt ctx
    | Some (WorkflowResult wf) ->
        use executor = new ExecutorAgent ()
        let! result = executor.Execute wf |> Async.StartAsTask
        return! json result nxt ctx
    | Some (_) -> return! handleUnexpectedBehavior nxt ctx
}

let handleGetComputations (nxt : HttpFunc) (ctx : HttpContext) = task {
    let repo = Computations.Repository.getRepository ()
    let availableComputationIds = repo.GetIdentifiers () |> List.map string

    return! json availableComputationIds nxt ctx
}

let securedApiRouter = scope {
    pipe_through (Auth.requireAuthentication JWT)
    get "/users/current" getCurrentUser
    get "/users/current/user-space" getUserSpace
    post "/users/current/user-space" createUserSpace
    post "/workspaces" createWorkspace
    getf "/workspaces/%O" getWorkspace

    // TODO currently delete does not work in Saturn
    // change when https://github.com/SaturnFramework/Saturn/pull/53 is merged and updated on nuget
    postf "/workspaces/%O" deleteWorkSpace

    getf "/workflows/%O" getWorkflow
    post "/workflows" createWorkflow
    post "/workflows/add-step" handleAddStep
    post "/workflows/change-step" handleChangeStep
    // I think right now get is appropriate as it is still synchronous and blocking
    getf "/workflows/%O/execute" executeWorkflow

    get "/computations" handleGetComputations }

let apiRouter = scope {
    pipe_through (pipeline { set_header "x-pipeline-type" "Api" })
    post "/token" handlePostToken
    forward "/secured" securedApiRouter }

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let browserRouter = scope {
    get "/" (htmlFile (Path.Combine(clientPath, "/index.html"))) }

let mainRouter = scope {
    forward "" browserRouter
    forward "/api" apiRouter }
