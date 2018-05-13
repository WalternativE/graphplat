module CQHandlers

open DataAccess
open Domain.Events
open Domain.Queries
open Domain.Projection

let userQueryHandler (userEs : EventStore<UserEvent>) =
    let handleQuery (query : UserQuery) =
        match query with
        | GetUser uId ->
            let ues = userEs.getEvents uId
            if List.length ues = 0 then
                None
            else
                ues
                |> List.head
                |> (fun ue ->
                        match ue with
                        | UserCreated u -> Some u
                        | _ -> None )
                |> Option.map (fun u -> userState ues u)

    handleQuery

// TODO optimize queries to go direclty against marten filters and projections
let spacesQueryHandler (spacesEs : EventStore<Event>) =
    let handleWorkflowQuery (wfq : WorkflowQuery) =
        match wfq with
        | GetWorkflow (uid, wfid) ->
            let evs =
                spacesEs.getEvents uid
            let uso =
                initialUserSpace evs
                |> Option.map (userSpacesState evs)

            uso
            |> Option.bind (fun us ->
                                us.Workspaces
                                |> List.tryFind (fun ws ->
                                                    match ws.Workflow with
                                                    | None -> false
                                                    | Some wf -> wf.Id = wfid)
                                |> Option.bind (fun ws -> ws.Workflow))

    let handleQuery (query : SpacesQuery) =
        match query with
        | GetUserspace uId ->
            let evs = spacesEs.getEvents uId
            if List.length evs = 0 then
                None
            else
                evs
                |> List.head
                |> (fun ev ->
                        match ev with
                        | UserSpaceCreated usp -> Some usp
                        | _ -> None )
                |> Option.map (fun usp -> userSpacesState evs usp |> UserSpaceResult)
        | GetWorkspace (uId, wsId) ->
            let evs = spacesEs.getEvents uId
            match evs with
            | [] -> None
            | history ->
                initialUserSpace history
                |> Option.bind (fun us ->
                        let cUs = userSpacesState history us
                        cUs.Workspaces
                        |> List.tryFind (fun w -> w.Id = wsId)
                        |> Option.map WorkspaceResult )
        | WorkflowQuery wfq ->
            handleWorkflowQuery wfq
            |> Option.map WorkflowResult

    handleQuery
