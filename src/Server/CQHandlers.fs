module CQHandlers

open DataAccess
open Domain.Events
open Domain.Queries
open Domain.Commands
open Domain.Projection
open Domain

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

let spacesQueryHandler (spacesEs : EventStore<Event>) =
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
                |> Option.map (fun usp -> userSpacesState evs usp)

    handleQuery

// let userComandHandler (userEs : EventStore<UserEvent>) =
//     let handleCommand (command : UserCommand) =
//         match command with
//         | CreateUser ->
//             let user

    // handleCommand

let commandHandler (spacesEs : EventStore<Event>)  =
    let query = spacesQueryHandler spacesEs

    let handleCommand (command : SpacesCommand) =
        match command with
        | CreateUserSpace user ->
            let space = GetUserspace user.Id |> query
            match space with
            | Some s ->
                let ev = AlreadyExists s |> Error
                spacesEs.writeEvent s.Id ev
                ev
            | None ->
                let s = Behaviour.createUserSpace user
                let ev = UserSpaceCreated s
                spacesEs.writeEvent s.Id ev
                ev

    handleCommand
